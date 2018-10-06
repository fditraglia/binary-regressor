# Title Slide
- Every student of introductory econometrics learns that a valid instrumental variable can serve double-duty: it can solve both the problem of endogeneity and classical measurement error simultaneously.
- Classical measurement error is an important special case: it requires that the true value of the regressor is *independent of* or at least uncorrelated with the measurement error.  
- Quite often in applied work the endogenous regressor of interest is in fact *binary* -- e.g. smoker vs. non-smoker or union vs. non-union employee. And a binary regressor *cannot* be measured with classical measurement error. If you are truly a 1, I can only mis-classify you *downwards* as a 0; if you are truly a 0, I can only mis-classify you *upwards* as a 1. This means that the truth must be *negatively* correlated with the measurement error.
- To accommodate binary, and indeed discrete, regressors, we need to consider a form of measurement error that allows correlation between the truth and the error. Today I'll work with so-called *non-differential* measurement error, which I'll describe in more detail in a few slides. But roughly speaking non-differential measurement error is *conditionally classical*. After conditioning on the right information, in particular the true value of the regressor, the component of the measurement error that is left over is unrelated to everything else.
- Today I want to pose a very simple question: if our regressor of interest is binary and subject to *nondifferential* measurement error, can a valid instrumental variable correct for *both* measurement error and endogeneity?
- While there are numerous papers that consider using an IV to correct for measurement error in a binary *exogenous* regressor, 

# Use a discrete IV to learn about b(x)
- Define sharp identified set: if beta is not point identified, we may still be able to use data to derive bounds. The sharp identified set refers to the best possible bounds.

# What is the effect of T*?
- Here is the specific model I will focus on today.
- READ SLIDE
- Mainly focus on this additively separable case, but a number of our results will also apply to a local average treatment effects (LATE) setting.

# Related Literature
- Maybe try to find a way to skip or streamline this slide. Perhaps incorporate the material elsewhere in the talk?


# Weak Identification
- Need to use fewer slides to explain this...
- This is *not* a weak instrument problem: that would be if the first stage probabilities P(T=1|z) don't vary much with z. But no matter how strong the first-stage relationship is, if beta is small, the additional moment equalities will contain very little information about a0 and a1.
- Today I will abstract from weak instrument considerations and assume that z is a *strong instrument*. Instead I'll take about the weak identification problem that arises from the mixture representation
