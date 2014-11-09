#include <zebi/bot.h>
#include <sstream>
#include <stdexcept>

#include <dirent.h>
#include <stdarg.h>
#include <gojira/parse_debug.h>
#include <zebi/json.h>

using namespace std;
using namespace bot; 

bool valid_channel_char( char c );
token_t *bot_builtin_send( stack_frame_t *frame );
token_t *bot_builtin_extern( stack_frame_t *frame );
token_t *bot_builtin_export( stack_frame_t *frame );
string tokenToString( token_t *token );
string procedureToString( token_t *token );

// XXX: no, just no
//      ...but for now it'll work.
static Bot *mainbot;

void bot::bot_error_printer( stack_frame_t *frame, char *fmt, ... ){
	va_list args;
	va_start( args, fmt );
	string channel;
	char *foo = new char[512];

	/*
	stack_trace( frame );
	vprintf( fmt, args );
	printf( "[%s] got here\n", __func__ );
	*/
	vsnprintf( foo, 512, fmt, args );
	channel = (string)((char *)mainbot->namespaces["bot"]->getVar( "channel" )->data);
	mainbot->server->sendLine( privmsg( channel, (string)foo ));

	delete foo;
	va_end( args );
}

Bot::Bot( string serv, string serv_port, string serv_nick, string log ){
	server = new Connection( serv, serv_port );

	if ( server->isConnected( )){
		mainbot = this;

		namespaces["global"] = new LispNamespace( );
		namespaces["bot"   ] = new LispNamespace( true, false );
		namespaces["intern"] = new LispNamespace( false, false );
		namespaces["dev"   ] = new LispNamespace( false, false );

		lisp_frame = namespaces["global"]->frame;
		global_add_func( lisp_frame, "send", bot_builtin_send );
		global_add_func( lisp_frame, "extern", bot_builtin_extern );
		global_add_func( lisp_frame, "export", bot_builtin_export );

		global_add_func( namespaces["dev"]->frame, "json-url", bot_builtin_json_url );

		loadScripts( "./scripts",        "global" );
		loadScripts( "./scripts/bot",    "bot" );
		loadScripts( "./scripts/intern", "intern" );

		logfile.open( "testlog.txt" );
		logfile << "Connected successfully." << endl;

		server->sendLine( usermsg( serv_nick ));
		server->sendLine( nickmsg( serv_nick ));

		iface_thread = thread( interface_loop, this );
	}
}

Bot::~Bot( ){
	iface_thread.join( );
	logfile.close( );
	delete server;
}

void Bot::loadScripts( std::string scriptdir, std::string nspace ){
	DIR *dir;
	struct dirent *ent;
	token_t *code;
	st_frame_t *frame = namespaces[nspace]->frame;

	if (( dir = opendir( scriptdir.c_str( ))) != NULL ){
		while (( ent = readdir( dir )) != NULL ){
			if (((string)ent->d_name).find( ".scm" ) != string::npos ){
				cout << "Have script " << scriptdir << "/" << ent->d_name << endl;
				ifstream scriptfile( scriptdir + "/" + ent->d_name );
				stringstream buf;
				buf << scriptfile.rdbuf( );

				namespaces[nspace]->runCode( buf.str( ));
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
			// Handle a possible command to the bot
			if ( msg.args[msg.data_start][0] == ':' ){
				logfile << "Got command " << msg.args[msg.data_start] << endl;

				string buf;
				unsigned i;
				for ( i = msg.data_start; i < msg.args.size( ); i++ ){
					buf = buf + msg.args[i] + " ";
				}

				buf = buf.substr( 1 );
				buf = "(" + buf + ")";

				namespaces["global"]->runCode( "(intern-set 'channel \"" + msg.channel + "\")" );
				namespaces["bot"   ]->runCode( "(intern-set 'channel \"" + msg.channel + "\")" );

				namespaces["global"]->runCode( "(intern-set 'nick \"" + msg.nick + "\")" );
				namespaces["bot"   ]->runCode( "(intern-set 'nick \"" + msg.nick + "\")" );

				namespaces["global"]->runCode( buf );

				if ( lisp_frame->end ){
					string tknstr = tokenToString( lisp_frame->end );

					if ( !tknstr.empty( )){
						server->sendLine( privmsg( msg.channel, tknstr ));
					}
				}

			// some channels require bots to respond to this command, good practice to do so anyways
			} else if ( msg.args[msg.data_start].substr(0,5) == ".bots" ){
				string botmsg = (string)"Reporting in! [\x03" + "4C/C++/Scheme" + "\x0f]";
				server->sendLine( privmsg( msg.channel, botmsg ));

			// rizon requires ctcp VERSION response
			} else if ( msg.args[msg.data_start].substr(0,9) == "\001VERSION\001" ){
				server->sendLine( privmsg( msg.nick, "\001VERSION Zebibot IRC framework 0.1\001" ));
			}

			// otherwise just ignore the message, probably wasn't important anyways
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
		frame->error_call( frame, "[%s] Error: Expected 2 arguments to \"send\"\n", __func__ );
	}

	ret->smalldata = val;

	return ret;
}

token_t *bot_builtin_extern( stack_frame_t *frame ){
	token_t *ret;
	token_t *move;
	token_t *namesp;
	token_t *var_name;
	bool val = false;
	string namesp_str;
	char *var_name_str;
	char *nick;

	ret = alloc_token( );
	ret->type = TYPE_NULL;
	ret->smalldata = 0;

	if ( frame->ntokens - 1 >= 2 ){
		namesp = frame->expr->next;
		var_name = frame->expr->next->next;

		if ( namesp->type == TYPE_SYMBOL && var_name->type == TYPE_SYMBOL ){
			namesp_str = (string)((char *)namesp->data);
			var_name_str = (char *)var_name->data;

			move = mainbot->namespaces["bot"]->getVar( "nick" );
			if ( move ){
				nick = (char *)move->data;

				try {
					if ( mainbot->namespaces.at( namesp_str )){
						if ( mainbot->namespaces[namesp_str]->readable ||
							 mainbot->namespaces[namesp_str]->canAccess( nick ))
						{
							ret = mainbot->namespaces[namesp_str]->getVar( var_name_str );
							ret = clone_tokens( ret );
						}
					}

				} catch ( const out_of_range &oor ){
					cerr << "Have bad namespace " << namesp_str << endl;
				}
			}
		}

	} else {
		frame->error_call( frame, "[%s] Error: Expected 2 arguments to \"extern\"\n", __func__ );
	}

	return ret;
}

token_t *bot_builtin_export( stack_frame_t *frame ){
	token_t *ret;
	token_t *move;
	token_t *namesp;
	token_t *var_name;
	token_t *var;
	bool val = false;
	string namesp_str;
	char *var_name_str;
	char *nick;

	ret = alloc_token( );
	ret->type = TYPE_NULL;
	ret->smalldata = 0;

	if ( frame->ntokens - 1 >= 3 ){
		namesp = frame->expr->next;
		var_name = frame->expr->next->next;
		var = frame->expr->next->next->next;

		if ( namesp->type == TYPE_SYMBOL && var_name->type == TYPE_SYMBOL ){
			namesp_str = (string)((char *)namesp->data);
			var_name_str = (char *)var_name->data;

			move = mainbot->namespaces["bot"]->getVar( "nick" );
			if ( move ){
				nick = (char *)move->data;

				try {
					mainbot->namespaces.at( namesp_str );
				} catch ( const out_of_range &oor ){
					frame->error_call( frame, "Creating new namespace \"%s\"\n", namesp_str.c_str( ));
					mainbot->namespaces[namesp_str] = new LispNamespace( );
				}

				if ( mainbot->namespaces[namesp_str]->writable ||
					 mainbot->namespaces[namesp_str]->canWrite( nick ))
				{
					mainbot->namespaces[namesp_str]->addVar( var_name_str, var );

				} else {
					frame->error_call( frame, "Can not export to \"%s\", permission denied\n", namesp_str.c_str( ));
				}
			}
		}

	} else {
		frame->error_call( frame, "[%s] Error: Expected 2 arguments to \"export\"\n", __func__ );
	}

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
			return std::to_string(((int)token->smalldata ));

		} else if ( token->type == TYPE_BOOLEAN ){
			return token->smalldata? "#t" : "#f";

		} else if ( token->type == TYPE_LIST ){
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

		} else if ( token->type == TYPE_PROCEDURE || token->type == TYPE_SYNTAX ){
			cerr << "Got here" << endl;

			token_t *move = token->down;
			string ret = "(";

			while ( move ){
				//ret = ret + tokenToString( move );
				ret = ret + procedureToString( move );
				if ( move->next )
					ret = ret + " ";
				move = move->next;
			}

			ret = ret + ")";
			return ret;

		} else if ( token->type == TYPE_CHAR ){
			return "#\\" + std::to_string( (char)token->smalldata );

		} else if ( token->type == TYPE_QUOTED_TOKEN ){
			return "'" + tokenToString( token->down );

		} else if ( token->type == TYPE_LAMBDA ){
			return "lambda";

		} else if ( token->type == TYPE_EXTERN_PROC ){
			return "external procedure";

		} else {
			return "";
		}

	} else {
		return "";
	}
}

string procedureToString( token_t *token ){
	if ( token ){
		if ( token->type == TYPE_SYMBOL ){
			if ( token->down ){
				return procedureToString( token->down );

			} else {
				return string((char *)token->data );
			}

		} else if ( token->type == TYPE_STRING ){
			cerr << "Got here" << endl;
			return "\"" + string((char *)token->data ) + "\"";

		} else if ( token->type == TYPE_NUMBER ){
			return std::to_string(((int)token->smalldata ));

		} else if ( token->type == TYPE_BOOLEAN ){
			return token->smalldata? "#t" : "#f";

		} else if ( token->type == TYPE_LIST || token->type == TYPE_PROCEDURE ){
			token_t *move = token->down;
			string ret = "(";

			while ( move ){
				ret = ret + procedureToString( move );
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
				ret = ret + procedureToString( move );
				if ( move->next )
					ret = ret + " ";
				move = move->next;
			}

			ret = ret + ")";
			return ret;

		} else if ( token->type == TYPE_CHAR ){
			return "#\\" + std::to_string( (char)token->smalldata );

		} else if ( token->type == TYPE_QUOTED_TOKEN ){
			return "'" + procedureToString( token->down );

		} else {
			return (string)(type_str( token->type ));
		}

	} else {
		return "";
	}
}
