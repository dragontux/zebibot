#include <zebi/bot.h>

using namespace std;
using namespace bot; 

Bot::Bot( string serv, string serv_port, string serv_nick, string log ){
	server = new Connection( serv, serv_port );

	if ( server->isConnected( )){
		cout << "Connected successfully." << endl;
		server->sendLine( usermsg( serv_nick ));
		server->sendLine( nickmsg( serv_nick ));
		server->sendLine( joinmsg( "#qwerqwerqwer" ));
	}
}

void Bot::mainLoop( ){
	bool running = true;

	while ( server->isConnected( ) && running ){
		IrcMessage msg( server->getLine( ));

		cout << msg.host << endl;
		cout << msg.action << endl;

		for ( unsigned i = msg.data_start; i < msg.args.size( ); i++ ){
			cout << msg.args[i] << " ";
		}

		cout << endl;

		if ( msg.action == "PING" ){
			server->sendLine( pingpong( msg ));
			cout << "Did ping" << endl;

		} else if ( msg.action == "PRIVMSG" && msg.args[msg.data_start][0] == '.' ){
			cout << "Got command " << msg.args[msg.data_start] << endl;
			server->sendLine( privmsg( msg.channel, "Aw yeah, got that" ));
		}

		//cerr << msg.msg << endl;
	}
}

Bot::~Bot( ){
	delete server;
}
