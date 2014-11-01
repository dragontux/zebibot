#include <zebi/bot.h>
#include <sstream>

#include <gojira/lexer.h>
#include <gojira/parser.h>
#include <gojira/config.h>
#include <gojira/runtime/runtime.h>
#include <gojira/runtime/builtin.h>
#include <gojira/runtime/garbage.h>
#include <dirent.h>

using namespace std;
using namespace bot; 

bool valid_channel_char( char c );
token_t *bot_builtin_send( stack_frame_t *frame );
string tokenToString( token_t *token );

// XXX: no, just no
//      ...but for now it'll work.
static Bot *mainbot;

Bot::Bot( string serv, string serv_port, string serv_nick, string log ){
	server = new Connection( serv, serv_port );

	if ( server->isConnected( )){
		mainbot = this;

		lisp_frame = frame_create( NULL, NULL );
		init_global_frame( lisp_frame );
		global_add_func( lisp_frame, ((string)"send").c_str( ), bot_builtin_send );

		loadScripts( "./scripts" );

		logfile.open( "testlog.txt" );
		logfile << "Connected successfully." << endl;

		server->sendLine( usermsg( serv_nick ));
		server->sendLine( nickmsg( serv_nick ));
		//server->sendLine( joinmsg( "#zebibot" ));

		iface_thread = thread( interface_loop, this );
	}
}

Bot::~Bot( ){
	iface_thread.join( );
	logfile.close( );
	delete server;
}

void Bot::loadScripts( std::string scriptdir ){
	DIR *dir;
	struct dirent *ent;
	token_t *code;

	if (( dir = opendir( scriptdir.c_str( ))) != NULL ){
		while (( ent = readdir( dir )) != NULL ){
			cout << ent->d_name << endl;

			if (((string)ent->d_name).find( ".scm" ) != string::npos ){
				cout << "Have script:" << endl;
				ifstream scriptfile( scriptdir + "/" + ent->d_name );
				stringstream buf;
				buf << scriptfile.rdbuf( );
				code = remove_punc_tokens( parse_tokens( lexerize( buf.str( ).c_str( ))));
				lisp_frame->ptr = code;
				eval_loop( lisp_frame, code );
			}
		}

		closedir( dir );

	} else {
		perror( scriptdir.c_str( ));
	}
}

void Bot::mainLoop( ){
	bool running = true;
	token_t *code = NULL;
	token_t *chancode = NULL;

	while ( server->isConnected( ) && running ){
		IrcMessage msg( server->getLine( ));

		if ( msg.action == "PING" ){
			server->sendLine( pingpong( msg ));
			logfile << "Did ping" << endl;

		} else if ( msg.action == "PRIVMSG" ){
			if ( msg.args[msg.data_start][0] == ':' ){
				logfile << "Got command " << msg.args[msg.data_start] << endl;

				string buf;
				unsigned i;
				for ( i = msg.data_start; i < msg.args.size( ); i++ ){
					buf = buf + msg.args[i] + " ";
				}
				buf = buf.substr( 1 );
				buf = "(" + buf + ")";

				logfile << buf << endl;

				code = remove_punc_tokens( parse_tokens( lexerize( buf.c_str( ))));
				if ( code ){
					// XXX: do this in a cleaner way
					chancode = remove_punc_tokens( parse_tokens( lexerize( ("(intern-set 'channel \"" + msg.channel + "\")").c_str( ))));
					lisp_frame->ptr = chancode;
					eval_loop( lisp_frame, code );

					chancode = remove_punc_tokens( parse_tokens( lexerize( ("(intern-set 'nick \"" + msg.nick + "\")").c_str( ))));
					lisp_frame->ptr = chancode;
					eval_loop( lisp_frame, code );

					lisp_frame->ptr = code;
					eval_loop_timed( lisp_frame, code, 100000 );

					if ( lisp_frame->end ){
						string tknstr = tokenToString( lisp_frame->end );

						if ( !tknstr.empty( )){
							server->sendLine( privmsg( msg.channel, tknstr ));
						}
					}
				}

			} else if ( msg.args[msg.data_start].substr(0,5) == ".bots" ){
				string botmsg = (string)"Reporting in! [\x03" + "4C/C++/Scheme" + "\x0f]";
				server->sendLine( privmsg( msg.channel, botmsg ));
			}
		}

		logfile << msg.msg << endl;
	}
}

bool valid_channel_char( char c ){
	return ( c >= 'a' && c <= 'z' ) || ( c >= 'A' && c <= 'Z' )
	      || c == '#' || c == '/' || c == '-';
}

token_t *bot_builtin_send( stack_frame_t *frame ){
	token_t *ret;
	token_t *move;
	token_t *channel;
	token_t *msg;
	bool val = false;
	bool valid_chan = true;
	char *channame;

	ret = alloc_token( );
	ret->type = TYPE_NULL;

	if ( frame->ntokens - 1 >= 2 ){
		channel = frame->expr->next;
		msg = frame->expr->next->next;

		if ( channel->type == TYPE_STRING ){
			channame = (char *)channel->data;

			if ( channame[0] == '#' ){
				for ( unsigned i = 0; channame[i]; i++ ){
					if ( !valid_channel_char( channame[i] )){
						valid_chan = false;
						break;
					}
				}

			} else {
				valid_chan = false;
			}

			if ( valid_chan ){
				string sbuf( "" );
				for ( move = msg; move; move = move->next ){
					sbuf += tokenToString( move );
				}

				mainbot->server->sendLine( privmsg( (string)((char *)channel->data), sbuf ));
			}
		}

	} else {
		printf( "[%s] Error: Expected 2 arguments to \"send\"\n", __func__ );
	}

	ret->smalldata = val;

	return ret;
}

string tokenToString( token_t *token ){
	if ( token ){
		if ( token->type == TYPE_STRING || token->type == TYPE_SYMBOL ){
			if ( token->down ){
				return tokenToString( token->down );

			} else {
				return string((char *)token->data );
			}

		} else if ( token->type == TYPE_NUMBER ){
			return std::to_string( token->smalldata );

		} else if ( token->type == TYPE_BOOLEAN ){
			return token->smalldata? "#t" : "#f";

		} else if ( token->type == TYPE_LIST || token->type == TYPE_PROCEDURE ){
			token_t *move = token->down;
			string ret = "(";

			while ( move ){
				ret = ret + tokenToString( move );
				if ( move->next )
					ret = ret + " ";
				move = move->next;
			}

			ret = ret + ")";
			return ret;

		} else if ( token->type == TYPE_PROCEDURE ){
			cerr << "Got here" << endl;

			token_t *move = token->down;
			string ret = "(";

			while ( move ){
				ret = ret + tokenToString( move );
				if ( move->next )
					ret = ret + " ";
				move = move->next;
			}

			ret = ret + ")";
			return ret;

		} else if ( token->type == TYPE_LAMBDA ){
			return "lambda";

		} else {
			return "";
		}

	} else {
		return "";
	}
}
