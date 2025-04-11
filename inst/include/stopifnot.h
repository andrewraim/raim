#ifndef STOPIFNOT_H
#define STOPIFNOT_H

#include <Rcpp.h>

namespace raim {

void stopifnot(bool cond, const char* format, ...)
{
	if (!cond) {
		// const std::string& msg = Rcpp::sprintf(format, ...);
		char msg[256];

		va_list arg;
		va_start(arg, format);
		int code = vsnprintf(msg, 256, format, arg);
 		va_end(arg);

		if (code < 0) {
			Rcpp::stop("Could not format error message");
		}

		Rcpp::stop("Error: %s is not TRUE", msg);
	}
}

void stopifnot(bool cond)
{
	if (!cond) {
		Rcpp::stop("Error: condition is not TRUE");
	}
}

}

#endif

