#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_VECTOR(x);
  PARAMETER(mu);
  PARAMETER(logSigma);

  Type f = 0;
  f -= dnorm(x, mu, exp(logSigma), true).sum();

  return f;
}
