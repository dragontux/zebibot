#include <zebi/bot.h>
#include <sstream>
using namespace std;
using namespace bot; 

int main( int argc, char *argv[] ){
	string nick, server, port;

	if ( argc == 4 ){
		nick = argv[1];
		server = argv[2];
		port = argv[3];

		Bot *ircBot = new Bot( server, port, nick, "test.log" );
		ircBot->mainLoop( );

		delete ircBot;

	} else {
		cout << "Usage: zebibot [nick] [server] [port]" << endl;
	}

	return 0;
}
