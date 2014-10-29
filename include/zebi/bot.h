#ifndef _ZEBI_BOT_H
#define _ZEBI_BOT_H

#include <iostream>
#include <fstream>
#include <vector>

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

class Bot {
	public:
		 Bot( std::string serv, std::string serv_port, std::string serv_nick, std::string log );
		~Bot( );
		void mainLoop( );
		Connection *server;

	private:
		std::ofstream logfile;
		std::string logname;
};

std::string privmsg( std::string recip, std::string data ); 
std::string nickmsg( std::string nick );
std::string usermsg( std::string nick );
std::string pingpong( IrcMessage msg );
std::string joinmsg( std::string chan );
std::string partmsg( std::string chan );

} // namespace bot

#endif
