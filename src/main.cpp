#include <zebi/bot.h>
#include <zebi/json.h>
#include <sstream>
#include <stdlib.h>
#include <gojira/tokens.h>

using namespace std;
using namespace bot; 

int main( int argc, char *argv[] ){
	//char *res = get_url( "http://api.duckduckgo.com/?q=linux&format=json&pretty=1&no_redirect=1&no_html=1&skip_disambig=1" );
	//cout << res << endl;
	//free( res );
	
	/*
	dump_tokens( get_json_url( "http://api.duckduckgo.com/?q=i3wm&format=json&pretty=1&no_redirect=1&no_html=1&skip_disambig=1" ));
	return 0;
	*/

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
