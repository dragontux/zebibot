#include <zebi/bot.h>
#include <linenoise/linenoise.h>

using namespace std;
using namespace bot; 

// TODO: split the big "if" statement into individual functions
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

                } else if ( args[0] == "add_read" ){
                    if ( args.size( ) == 3 ){
                        try {
                            if ( dabot->namespaces.at( args[1] )){
                                dabot->namespaces[args[1]]->addAllowedNick( args[2] );
                            }

                        } catch ( const out_of_range &oor ){
                            cerr << "Have bad namespace " << args[1] << endl;
                        }

                    } else {
						cout << "Usage: add_read [namespace] [nick]" << endl;
                    }

                } else if ( args[0] == "add_write" ){
                    if ( args.size( ) == 3 ){
                        try {
                            if ( dabot->namespaces.at( args[1] )){
                                dabot->namespaces[args[1]]->addWriteNick( args[2] );
                            }

                        } catch ( const out_of_range &oor ){
                            cerr << "Have bad namespace " << args[1] << endl;
                        }

                    } else {
						cout << "Usage: add_write [namespace] [nick]" << endl;
                    }

                } else if ( args[0] == "rm_read" ){
                    if ( args.size( ) == 3 ){
                        try {
                            if ( dabot->namespaces.at( args[1] )){
                                dabot->namespaces[args[1]]->rmAllowedNick( args[2] );
                            }

                        } catch ( const out_of_range &oor ){
                            cerr << "Have bad namespace " << args[1] << endl;
                        }

                    } else {
						cout << "Usage: rm_read [namespace] [nick]" << endl;
                    }

                } else if ( args[0] == "rm_write" ){
                    if ( args.size( ) == 3 ){
                        try {
                            if ( dabot->namespaces.at( args[1] )){
                                dabot->namespaces[args[1]]->rmWriteNick( args[2] );
                            }

                        } catch ( const out_of_range &oor ){
                            cerr << "Have bad namespace " << args[1] << endl;
                        }

                    } else {
						cout << "Usage: rm_write [namespace] [nick]" << endl;
                    }
                } else if ( args[0] == "namespaces" ){
                    map<string, LispNamespace *>::iterator iter;

                    for ( iter = dabot->namespaces.begin( ); iter != dabot->namespaces.end(); iter++ ){
                        cout << iter->first << endl;
                        if ( dabot->namespaces[iter->first]->readable ){
                            cout << "  [globally readable]" << endl;

                        } else {
                            cout << "  read:  ";
                            for ( unsigned i = 0;
                                i < dabot->namespaces[iter->first]->nicks_allowed.size( );
                                i++ )
                            {
                                cout << dabot->namespaces[iter->first]->nicks_allowed[i]
                                     << ", ";
                            }

                            cout << endl;
                        }

                        if ( dabot->namespaces[iter->first]->writable ){
                            cout << "  [globally writable]" << endl;

                        } else {
                            cout << "  write: ";
                            for ( unsigned i = 0;
                                i < dabot->namespaces[iter->first]->nicks_writable.size( );
                                i++ )
                            {
                                cout << dabot->namespaces[iter->first]->nicks_writable[i]
                                     << ", ";
                            }

                            cout << endl;
                        }
                        
                    }

                } else if ( args[0] == "ignore_nick" ){
                    if ( args.size( ) == 2 ){
                        dabot->ignore.nicks.push_back( args[1] );

                    } else {
						cout << "Usage: ignore_nick [nick]" << endl;
                    }

                } else if ( args[0] == "unignore_nick" ){
                    size_t i;

                    if ( args.size( ) == 2 ){
                        for ( i = 0; i < dabot->ignore.nicks.size( ); i++ ){
                            if ( dabot->ignore.nicks[i] == args[1] ){
                                dabot->ignore.nicks[i].erase( i );
                                break;
                            }
                        }

                    } else {
						cout << "Usage: unignore_nick [nick]" << endl;
                    }

				} else {
					cout << "Undefined command" << endl;
				}
			}
		}
	}
}
