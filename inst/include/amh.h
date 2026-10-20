#ifndef AMH_H
#define AMH_H

#include "Rcpp.h"

/*
* Adaptive Metropolis-Hastings (AMH) for univariate chains from Haario,
* Saksman, & Tamminen (Computational Statistics, 2005), as presented in Section
* 3.1 of Roberts & Rosenthal (JCGS, 2009).
*/

class amh
{
public:
	amh(double x0)
	: _x_prev(x0), _mean(0), _rep(0), _g(0), _v_prop(0)
	{
	}

	double draw();
	Rcpp::NumericVector amh::draw(unsigned int n);

/* Public fields for access and mutate */
	std::function<double(double)> log_target = [](double x) -> double {
		Rcpp::stop("Variable log_target was not set");
		return 0;
	};
	unsigned int tune_start = 10;
	unsigned int tune_end = std::numeric_limits<unsigned int>::max();
	double v_init = 25;
	double c = 2.4;
	double eps = 0.05;

private:
	double _x_prev;
	double _mean;
	unsigned int _rep;
	double _g;
	double _v_prop;
};

inline double amh::draw()
{
	// Draw a candidate and decide whether to accept it
	double x_prev = _x_prev;
	double u = R::runif(0, 1);
	double x_prop = R::rnorm(x_prev, std::sqrt(_v_prop));
	double log_num = log_target(x_prop);
	double log_den = log_target(x_prev);
	double log_ratio = std::min(log_num - log_den, 0.0);
	double out = (std::log(u) < log_ratio) ? x_prop : x_prev;

	// Update the mean of draw history with recursive formula
	double x_mean_prev = _mean;
	_mean = (_rep * _mean + out) / (_rep + 1);

	// Update the variance of draw history with recursive formula
	if (_rep == 0) {
		_g = std::pow(out, 2) / (_rep + 1);
	} else  {
		_g = (_rep - 1) / _rep * _g +
			std::pow(x_mean_prev, 2) +
			std::pow(out, 2) / _rep -
			(_rep + 1) / _rep * std::pow(_mean, 2);
	}

	// Update the proposal variance
	if (_rep < tune_start) {
		_v_prop = v_init;
	} else if (_rep < tune_end) {
		_v_prop = std::pow(c, 2)  * (_g + eps);
	}

	_rep++;
	return out;
}

inline Rcpp::NumericVector amh::draw(unsigned int n)
{
	Rcpp::NumericVector out(n);
	for (unsigned int i = 0; i < n; i++) {
		out(i) = draw();
	}

	return out;
}

#endif

