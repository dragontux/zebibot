#include <zebi/bot.h>
#include <vector>
#include <sstream>

using namespace std;
using namespace bot;

// shamelessly stolen from stackoverflow
vector<string> &bot::split( const string &s, char delim, vector<string> &elems ){
	stringstream ss(s);
	string item;

	while( getline( ss, item, delim )){
		elems.push_back( item );
	}

	return elems;
}

vector<string> bot::split( const string &s, char delim ){
	vector<string> elems;
	split( s, delim, elems );
	return elems;
}
// </shameless>

IrcMessage::IrcMessage( string raw ){
	valid = false;
	msg = raw;
	data_start = 0;
	vector<string> elem;

	if ( !raw.empty( )){
		elem = split( raw, ' ' );

		if ( !elem.empty( )){
			unsigned i;

			for ( i = 0; i < elem.size( ); i++ ){
				if ( elem[i][0] == ':' ){
					if ( i == 0 ){
						// TODO: host parsing
						vector<string> tmp = split( elem[0].substr( 1 ), '!' );
						if ( tmp.size( ) == 2 ){
							nick = tmp[0];
							host = tmp[1];
						}

					} else {
						elem[i] = elem[i].substr( 1 );
						data_start = i;
						
						break;
					}
				}
			}

			// if no host was specified, the action is the first slice
			if ( host.empty( )){
				action = elem[0];
				

			// otherwise it's the second
			} else {
				action = elem[1];
				channel = elem[2];
			}
		}

		args = elem;
	}
}

IrcMessage::~IrcMessage( ){

}

bool IrcMessage::isValid( ){
	return valid;
}


string bot::privmsg( string recip, string data ){
	return "PRIVMSG " + recip + " :" + data + "\r\n";
}

string bot::nickmsg( string nick ){
	return "NICK " + nick + "\r\n";
}

string bot::usermsg( string nick ){
	return "USER " + nick + " " + nick + " " + nick + " :" + nick + "\r\n";
}

string bot::joinmsg( string chan ){
	return "JOIN " + chan + "\r\n";
}

string bot::partmsg( string chan ){
	return "PART " + chan + "\r\n";
}

string bot::notice( string recip, string data ){
	return "NOTICE " + recip + " :" + data + "\r\n";
}

string bot::pingpong( IrcMessage msg ){
	return "PONG :" + msg.args[msg.data_start];
}
