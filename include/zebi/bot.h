#ifndef _ZEBI_BOT_H
#define _ZEBI_BOT_H

#include <iostream>
#include <fstream>
#include <vector>
#include <thread>
#include <map>
#include <gojira/runtime/runtime.h>

// TODO: split this into seperate headers sometime

namespace bot {

class Connection;
class Bot;

class IrcMessage {
	public:
		 IrcMessage( std::string raw );
		~IrcMessage( );
		bool isValid( );

		std::string nick;
		std::string host;
		std::string action;
		std::string channel;
		unsigned data_start;

		std::vector<std::string> args;
		std::string msg;

	private:
		bool valid;
};

class Connection {
	public:
		 Connection( std::string serv, std::string serv_port );
		~Connection( );
		bool isConnected( );

		std::string getLine( );
		int sendLine( std::string line );

	private:
		std::string server;
		std::string port;
		int sock;
		unsigned last_ping;
		bool connected;
};

class LispNamespace {
	public:
		 LispNamespace( );
		 LispNamespace( bool allow_read, bool allow_write );
		~LispNamespace( );

		bool canAccess( std::string nick );
		bool canWrite( std::string nick );

		bool addAllowedNick( std::string nick );
		bool addWriteNick( std::string nick );
		bool rmAllowedNick( std::string nick );
		bool rmWriteNick( std::string nick );
		bool addVar( std::string name, token_t *tree );
		token_t *getVar( std::string name );
		token_t *runCode( std::string code );

		std::vector<std::string> nicks_allowed;
		std::vector<std::string> nicks_writable;
		bool readable, writable;
		st_frame_t *frame;

	private:
};

class Bot {
	public:
		 Bot( std::string serv, std::string serv_port, std::string serv_nick, std::string log );
		~Bot( );
		void mainLoop( );
		void joinChan( std::string channel );
		LispNamespace *getNamespace( std::string name );

		Connection *server;

		std::map<std::string, LispNamespace *> namespaces;
		std::vector<std::string> channels;
		st_frame_t *lisp_frame;

	private:
		void loadScripts( std::string scriptdir, std::string nspace );

		std::ofstream logfile;
		std::string logname;
		std::thread iface_thread;
};

void interface_loop( Bot *dabot );
void bot_error_printer( stack_frame_t *frame, char *fmt, ... );

std::string privmsg( std::string recip, std::string data ); 
std::string nickmsg( std::string nick );
std::string usermsg( std::string nick );
std::string pingpong( IrcMessage msg );
std::string joinmsg( std::string chan );
std::string partmsg( std::string chan );

std::vector<std::string> &split( const std::string &s, char delim, std::vector<std::string> &elems );
std::vector<std::string>  split( const std::string &s, char delim );

} // namespace bot

#endif
