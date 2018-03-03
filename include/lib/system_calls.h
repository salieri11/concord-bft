#include <string>

using namespace std;

string constructExternalCallError(string command, int popenErrNo, int exitCode);
string makeExternalCall(string command);
