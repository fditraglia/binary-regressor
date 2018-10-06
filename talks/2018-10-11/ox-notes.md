# Title Slide
- Every student of introductory econometrics learns that a valid instrumental variable can serve double-duty: it can solve both the problem of endogeneity and classical measurement error simultaneously.
- Classical measurement error is an important special case: it requires that the true value of the regressor is *independent of* or at least uncorrelated with the measurement error.  
- Quite often in applied work the endogenous regressor of interest is in fact *binary* -- e.g. smoker vs. non-smoker or union vs. non-union employee. And a binary regressor *cannot* be measured with classical measurement error. If you are truly a 1, I can only mis-classify you *downwards* as a 0; if you are truly a 0, I can only mis-classify you *upwards* as a 1. This means that the truth must be *negatively* correlated with the measurement error.
- To accommodate binary, and indeed discrete, regressors, we need to consider a form of measurement error that allows correlation between the truth and the error. Today I'll work with so-called *non-differential* measurement error, which I'll describe in more detail in a few slides. But roughly speaking non-differential measurement error is *conditionally classical*. After conditioning on the right information, in particular the true value of the regressor, the component of the measurement error that is left over is unrelated to everything else.
- Today I want to pose a very simple question: if our regressor of interest is binary and subject to *nondifferential* measurement error, can a valid instrumental variable correct for *both* measurement error and endogeneity?

# What is the effect of T*?
- Here is the specific model I will focus on today.
- READ SLIDE
- Mainly focus on this additively separable case, but a number of our results will also apply to a local average treatment effects (LATE) setting.

# Use a discrete IV to learn about b(x)
- Define sharp identified set: if beta is not point identified, we may still be able to use data to derive bounds. The sharp identified set refers to the best possible bounds.

# Example: Smoking and Birthweight (SNAP Trial)
- Perhaps replace this with the Burde and Linden example so that I can show some pictures for the bounds in that case. I should have that slides somewhere in one of my older talks...

# Related Literature
- Ditch this slide to save time. Mention the most closely related papers below in relation to my own.

# Baseline Assumptions I - Model and Instrument
- Because this is an econometrics talk, there will be lots of assumptions! But I want to make sure it's clear what each group of assumptions is actually doing.
- This slide and the next one detail what I will call the "baseline" assumptions, which I will maintain throughout the talk.
- First part of the baseline assumptions says: "If T* were observed, then the model would be identified." These are just the usual IV relevance and validity conditions applied to the *infeasible* model that uses the unobserved true regressor T*

# Baseline Assumptions II - Measurement Error
- Second part of the baseline assumptions concerns the measurement error process.
- But first some notation: mis-classification probabilities a0 and a1. The subscript gives the value of the "truth"
- Three conditions on the mis-classification.
- First is that, conditional on x, the mis-classification rates do not depend on z. This is not an innocuous assumption. Give an example when it holds and when it doesn't. Nearly impossible to make any progress without this assumption. How reasonable it is depends on the choice of conditioning variables x.
- Second measurement error assumption is much less controversial and also much less consequential: assume that T is positively correlated with T*. This turns out to be equivalent to requiring that the sum of the mis-classification probabilities is less than one. Note that since these are *conditional* probabilities that condition on different events, they *could* sume to more than one. 
- I'll talk about what happens if we relax the second assumption in a few slides. The bare minimum that we need is that T is correlated with T*. This is pretty reasonable: if T contains no information about T* then there's clearly no way we can proceed!
- Third measurement error assumption is the *non-differential assumption*. This version of the assumption concerns first moments. Stated in terms of epsilon, but intuitively what this really says is that the conditional mean of Y does not depend on T after controlling for x, z, and T*. Like the first of the measurement error assumptions, the plausibility of this assumption depends on the situation and also the controls included in x.
- An example of what non-differential measurement error rules out is "returns to lying." For example, suppose Y is log wage, T* is the true indicator for whether you graduated from college, and z is an instrument for college attendance. If T is an individual's *self-report* 
- whether you had a college degree,  which is incidentally the topic of a paper I'm currently writing with Arthur Lewbel.


# Existing Result for Endogenous T* is Incorrect
- While there are numerous papers that consider using an IV to correct for measurement error in a binary *exogenous* regressor, the only existing result that considers the *endogenous* regressor case is incorrect.
- Explain that the main point of the Mahajan paper is not 

# Weak Identification
- Need to use fewer slides to explain this...
- This is *not* a weak instrument problem: that would be if the first stage probabilities P(T=1|z) don't vary much with z. But no matter how strong the first-stage relationship is, if beta is small, the additional moment equalities will contain very little information about a0 and a1.
- Today I will abstract from weak instrument considerations and assume that z is a *strong instrument*. Instead I'll take about the weak identification problem that arises from the mixture representation
