#include <zebi/bot.h>
#include <linenoise/linenoise.h>

using namespace std;
using namespace bot; 

void bot::interface_loop( Bot *dabot ){
	while ( true ){
		string in( linenoise( "zebibot> " ));

		if ( !in.empty( )){
			linenoiseHistoryAdd( in.c_str( ));
			vector<string> args = split( in, ' ' );

			if ( !args.empty( )){
				if ( args[0] == "help" ){
					cout << "There is no god." << endl;
				} else if ( args[0] == "join"){
					if ( args.size( ) >= 2 ){
						dabot->server->sendLine( joinmsg( args[1] ));

					} else {
						cout << "Usage: join [channel] [channel] ..." << endl;
					}

				} else if ( args[0] == "part"){
					if ( args.size( ) >= 2 ){
						dabot->server->sendLine( partmsg( args[1] ));

					} else {
						cout << "Usage: part [channel] [channel] ..." << endl;
					}

				} else if ( args[0] == "say" ){
					if ( args.size( ) >= 3 ){
						string buf = "";

						for ( unsigned i = 2; i < args.size( ); i++ ){
							buf += args[i] + " ";
						}

						dabot->server->sendLine( privmsg( args[1], buf ));

					} else {
						cout << "Usage: say [channel] [message]" << endl;
					}

				} else {
					cout << "Undefined command" << endl;
				}
			}
		}
	}
}
