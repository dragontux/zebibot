#include <zebi/bot.h>
#include <gojira/lexer.h>
#include <gojira/parser.h>
#include <gojira/config.h>
#include <gojira/runtime/runtime.h>
#include <gojira/runtime/builtin.h>
#include <gojira/runtime/garbage.h>
using namespace std;
using namespace bot;

LispNamespace::LispNamespace( ){
	frame = frame_create( NULL, NULL );
	init_global_frame( frame );
	frame->error_call = bot_error_printer;

	readable = writable = true;
}

LispNamespace::LispNamespace( bool allow_read, bool allow_write ){
	frame = frame_create( NULL, NULL );
	init_global_frame( frame );
	frame->error_call = bot_error_printer;

	readable = allow_read;
	writable = allow_write;
}

LispNamespace::~LispNamespace( ){
	frame_free( frame );
}

bool LispNamespace::canAccess( std::string nick ){
	bool ret = false;

	for ( unsigned i = 0; i < nicks_allowed.size( ); i++ ){
		if ( nick == nicks_allowed[i] ){
			ret = true;
			break;
		}
	}

	return ret;
}

bool LispNamespace::canWrite( std::string nick ){
	bool ret = false;

	for ( unsigned i = 0; i < nicks_writable.size( ); i++ ){
		if ( nick == nicks_writable[i] ){
			ret = true;
			break;
		}
	}

	return ret;
}

bool LispNamespace::addAllowedNick( std::string nick ){
	nicks_allowed.push_back( nick );

	return true;
}

bool LispNamespace::addWriteNick( std::string nick ){
	nicks_writable.push_back( nick );

	return true;
}

bool LispNamespace::rmAllowedNick( std::string nick ){
	for ( unsigned i = 0; i < nicks_allowed.size( ); i++ ){
		if ( nicks_allowed[i] == nick ){
			nicks_allowed.erase( nicks_allowed.begin( ) + i );
			break;
		}
	}

	return true;
}

bool LispNamespace::rmWriteNick( std::string nick ){
	for ( unsigned i = 0; i < nicks_writable.size( ); i++ ){
		if ( nicks_writable[i] == nick ){
			nicks_writable.erase( nicks_writable.begin( ) + i );
			break;
		}
	}

	return true;
}

bool LispNamespace::addVar( std::string name, token_t *tree ){
	frame_add_var( frame, name.c_str( ), tree );

	return true;
}

token_t *LispNamespace::getVar( std::string name ){
	token_t *ret;

	ret = frame_find_var( frame, name.c_str( ));

	return ret;
}

token_t *LispNamespace::runCode( std::string code ){
	token_t *ret = NULL;
	token_t *chancode;

	chancode = remove_punc_tokens( parse_tokens( lexerize( code.c_str( ))));
	if ( chancode ){
		frame->ptr = chancode;
		//eval_loop( frame, chancode );
		eval_loop_timed( frame, chancode, 100000 );

		ret = frame->end;
	}

	return ret;
}
