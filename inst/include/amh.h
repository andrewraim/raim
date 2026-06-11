#include "RcppArmadillo.h"

class amh
{
public:
	amh(double x0)
	: _x_prev(x0), _x_mean(0), _rep(0), _x_g(0), _v_prop(0)
	{
	}

	void update(const std::function<double(double)>& f);
	double draw(unsigned int report);

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
	double _x_mean;
	unsigned int _rep;
	double _x_g;
	double _v_prop;
};

inline double amh::draw(unsigned int report)
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
	double x_mean_prev = _x_mean;
	_x_mean = (_rep * _x_mean + out) / (_rep + 1);

	// Update the variance of draw history with recursive formula
	if (_rep == 0) {
		_x_g = std::pow(out, 2) / (_rep + 1);
	} else  {
		_x_g = (_rep - 1) / _rep * _x_g +
			std::pow(x_mean_prev, 2) +
			std::pow(out, 2) / _rep -
			(_rep + 1) / _rep * std::pow(_x_mean, 2);
	}

	// Update the proposal variance
	if (_rep < tune_start) {
		_v_prop = v_init;
	} else if (_rep < tune_end) {
		_v_prop = std::pow(c, 2)  * (_x_g + eps);
	}

	_rep++;
	return out;
}
