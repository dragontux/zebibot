#include <zebi/bot.h>

#include <string.h>

#include <sys/socket.h>
#include <sys/types.h>
#include <netdb.h>
#include <unistd.h>

using namespace std;
using namespace bot;

Connection::Connection( string serv, string serv_port ){
	struct addrinfo hints, *res = NULL;

	memset( &hints, 0, sizeof( hints ));
	hints.ai_family   = AF_UNSPEC;
	hints.ai_socktype = SOCK_STREAM;
	hints.ai_flags    = AI_PASSIVE;

	if ( getaddrinfo( serv.c_str( ), serv_port.c_str( ), &hints, &res )       >= 0 &&
	   ( sock = socket( res->ai_family, res->ai_socktype, res->ai_protocol )) >= 0 &&
	   ( connect( sock, res->ai_addr, res->ai_addrlen ))                      >= 0 )
	{
		server = serv;
		port = serv_port;

		connected = true;

	} else {
		perror( "error" );
		cerr << "Could not connect to " << serv << ":" << serv_port << endl;
	}
}

Connection::~Connection( ){
	if ( connected ){
		close( sock );
	}
}

bool Connection::isConnected( ){
	return connected;
}

string Connection::getLine( ){
	char *line = new char[1024];
	string ret;
	int len;

	if (( len = recv( sock, line, 1024, 0 )) == 0 ){
		connected = false;
	}

	line[len] = 0;

	ret = string( line );
	delete line;

	return ret;
}

int Connection::sendLine( string line ){
	int len;
	unsigned to_send;

	if ( line.length( ) >= 510 ){
		line = line.substr( 0, 510 ) + "\r\n";
		to_send = 512;

	} else {
		to_send = line.length( );
	}

	len = send( sock, line.c_str( ), to_send, 0 );

	return len;
}
