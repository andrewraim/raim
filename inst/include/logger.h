#ifndef LOGGER_H
#define LOGGER_H

#include <Rcpp.h>

namespace raim {

/*
* Default date format and separator. We may be able to delegate to the more
* general function, but initial attempts to do this seem to be messing up the
* variable argument construct.
*/
int logger(const char* fmt, ...)
{
	// Print time and separator
	time_t timer;
	char buffer[64];
	struct tm* tm_info;

	timer = time(NULL);
	tm_info = localtime(&timer);

	strftime(buffer, 64, "%Y-%m-%d %H:%M:%S", tm_info);
	printf("%s%s", buffer, " - ");

	// Call printf to handle the rest of the message
	va_list arg;
	va_start(arg, fmt);
	int out = vprintf(fmt, arg);
	va_end(arg);
	return out;
}

int logger(const char* dt_fmt, const char* sep, const char* fmt, ...)
{
	// Print time and separator
	time_t timer;
	char buffer[64];
	struct tm* tm_info;

	timer = time(NULL);
	tm_info = localtime(&timer);

	strftime(buffer, 64, dt_fmt, tm_info);
	printf("%s%s", buffer, sep);

	// Call printf to handle the rest of the message
	va_list arg;
	va_start(arg, fmt);
	int out = vprintf(fmt, arg);
	va_end(arg);
	return out;
}

}

#endif
