#ifndef _ZEBI_JSON_H
#define _ZEBI_JSON_H
#ifdef __cplusplus
extern "C" {
#endif

#include <gojira/tokens.h>
#include <gojira/runtime/frame.h>

char *get_url( const char *url );
token_t *get_json_url( const char *url );
token_t *bot_builtin_json_url( stack_frame_t *frame );

#ifdef __cplusplus
}
#endif
#endif
