#include <zebi/json.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>

#include <curl/curl.h>
#include <jansson.h>

typedef struct memblock {
	char *mem;
	size_t size;
} memblock_t;

static size_t write_memblock( void *contents, size_t size, size_t nmemb, void *userp ){
	size_t ret = 0;
	size_t real = size * nmemb;
	memblock_t *mem = (memblock_t *)userp;

	mem->mem = realloc( mem->mem, mem->size + real + 1 );
	if ( mem->mem ){
		memcpy( mem->mem + mem->size, contents, real );
		mem->size += real;
		mem->mem[mem->size] = 0;

		ret = real;

	} else {
		printf( "[%s] out of memory\n", __func__ );
	}

	return ret;
}

char *get_url( const char *url ){
	CURL *curl_handle;
	CURLcode res;
	char *thing;
	unsigned i;

	memblock_t block;

	block.mem  = malloc( 1 );
	block.size = 0;

	thing = strdup( url );
	for ( i = 0; thing[i]; i++ ){
		if ( thing[i] == ' ' )
			// XXX: Preferably this should be replace with the proper escape, %20
			thing[i] = '+';
	}

	curl_handle = curl_easy_init( );
	curl_easy_setopt( curl_handle, CURLOPT_URL,           url );
	curl_easy_setopt( curl_handle, CURLOPT_WRITEFUNCTION, write_memblock );
	curl_easy_setopt( curl_handle, CURLOPT_WRITEDATA,     (void *)&block );
	curl_easy_setopt( curl_handle, CURLOPT_USERAGENT,     "libcurl/1.0" );

	res = curl_easy_perform( curl_handle );

	if ( res != CURLE_OK ){
		printf( "[%s] Some failure in curl\n", __func__ );

		free( block.mem );
		block.mem = NULL;
	}

	curl_global_cleanup( );

	return block.mem;
}

void dump_json( json_t *data ){
	size_t i;
	json_t *temp;
	char *key;

	if ( data ){
		switch( json_typeof( data )){
			case JSON_OBJECT:
				printf( "(object (");
				json_object_foreach( data, key, temp ){
					printf( "(" );
					printf( "%s", key );
					printf( " " );
					dump_json( temp );
					printf( ") " );
				}
				printf( "))");

				break;

			case JSON_ARRAY:
				printf( "(array (" );
				json_array_foreach( data, i, temp ){
					//printf( "%d : ", i );
					dump_json( temp );
					printf( " " );
				}
				printf( "))" );

				break;

			case JSON_STRING:
				printf( "\"%s\"", json_string_value( data ));
				break;

			case JSON_INTEGER:
				printf( "%d", json_integer_value( data ));
				break;

			case JSON_REAL:
				printf( "%f", json_real_value( data ));
				break;

			case JSON_TRUE:
				printf( "#t" );
				break;

			case JSON_FALSE:
				printf( "#f" );
				break;

			case JSON_NULL:
				printf( "'null" );
				break;

			default:
				printf( "/|\\ something went wrong, unknown type\n" );
				break;
		}
	}
}

token_t *json_to_token( json_t *data ){
	size_t i;
	json_t *temp;
	char *key;

	token_t *ret, *move, *foo, *last = NULL;
	ret = NULL;

	if ( data ){
		ret = calloc( 1, sizeof( token_t ));
		ret->type = TYPE_NULL;

		switch( json_typeof( data )){
			case JSON_OBJECT:
				ret->type = TYPE_LIST;
				ret->down       = calloc( 1, sizeof( token_t ));
				ret->down->type = TYPE_SYMBOL;
				ret->down->data = strdup( "object" );

				ret->down->next       = calloc( 1, sizeof( token_t ));
				ret->down->next->type = TYPE_LIST;
				move = NULL;

				json_object_foreach( data, key, temp ){
					foo = calloc( 1, sizeof( token_t ));
					foo->type = TYPE_LIST;
					// keep track of where the first and last tokens in the list are
					move = move? move : foo;
					if ( last ){
						last = last->next = foo;
					} else {
						last = foo;
					}

					foo->down = calloc( 1, sizeof( token_t ));
					foo->down->type = TYPE_SYMBOL;
					foo->down->data = strdup( key );
					foo->down->next = json_to_token( temp );
				}

				ret->down->next->down = move;

				break;

			case JSON_ARRAY:
				ret->type = TYPE_LIST;
				ret->down       = calloc( 1, sizeof( token_t ));
				ret->down->type = TYPE_SYMBOL;
				ret->down->data = strdup( "array" );

				ret->down->next       = calloc( 1, sizeof( token_t ));
				ret->down->next->type = TYPE_LIST;
				move = NULL;

				json_array_foreach( data, i, temp ){
					foo = json_to_token( temp );

					move = move? move : foo;
					if ( last ){
						last = last->next = foo;
					} else {
						last = foo;
					}
				}
				ret->down->next->down = move;

				break;

			case JSON_STRING:
				ret->type = TYPE_STRING;
				ret->data = strdup( json_string_value( data ));
				//printf( "\"%s\"", json_string_value( data ));
				break;

			case JSON_INTEGER:
				ret->type = TYPE_NUMBER;
				ret->smalldata = json_integer_value( data );
				//printf( "%d", json_integer_value( data ));
				break;

			case JSON_REAL:
				ret->type = TYPE_NUMBER;
				ret->smalldata = (int)json_real_value( data );
				//printf( "%f", json_real_value( data ));
				break;

			case JSON_TRUE:
				ret->type = TYPE_BOOLEAN;
				ret->smalldata = true;
				//printf( "#t" );
				break;

			case JSON_FALSE:
				ret->type = TYPE_BOOLEAN;
				ret->smalldata = false;
				//printf( "#f" );
				break;

			case JSON_NULL:
				//printf( "'null" );
				break;

			default:
				printf( "/|\\ something went wrong, unknown type\n" );
				break;
		}
	}

	return ret;
}

token_t *get_json_url( const char *url ){
	size_t i;
	char *text;
	token_t *ret = NULL;

	json_t *root;
	json_error_t error;

	if (( text = get_url( url ))){
		if (( root = json_loads( text, 0, &error ))){
			//dump_json( root );

			ret = json_to_token( root );

			json_decref( root );
		}

		free( text );
	}

	return ret;
}

token_t *bot_builtin_json_url( stack_frame_t *frame ){
	token_t *ret;
	token_t *url_tok;
	char *url;

	ret = alloc_token( );
	ret->type = TYPE_NULL;
	ret->smalldata = false;

	if ( frame->ntokens - 1 == 1 ){
		url_tok = frame->expr->next;

		if ( url_tok->type == TYPE_STRING ){
			url = (char *)url_tok->data;

			if ( url ){
				ret = get_json_url( url );
			}
		}

	} else {
		frame->error_call( frame, "[%s] Error: Expected 1 arguments to \"json-url\" but have %d\n", __func__, frame->ntokens - 1 );
	}

	return ret;
}
