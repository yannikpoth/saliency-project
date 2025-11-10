# New Models - Factorial Design (6 Models)

**Date**: 2025-11-07
**Status**: Ready for testing
**Parameterization**: Centered Parameterization (CP)

## Overview

This directory contains 6 reinforcement learning models implementing a **factorial design** to test the effects of salient feedback on:
1. **Learning rate modulation** (`alpha_shift`)
2. **Perseveration/choice stickiness modulation** (`kappa_shift`)

## Updated Prior Specifications

All models follow the professor's updated specifications:

- **Group-level means** (alpha, beta): `uniform(-3, 3)` ⚠️ Changed from (-4, 4)
- **All SD priors**: `uniform(0.0001, 10)` ⚠️ Changed from (0.001, 3)
- **Shift priors** (alpha_shift, kappa_shift): `normal(0, 1)` ✓
- **Kappa prior**: `normal(0, 1)` with **Phi transformation** ✓
- **Beta transformation**: `Phi(beta_raw) * 10` ✓
- **Kappa transformation**: `Phi(kappa_raw)` to [0, 1] scale ✓

## Factorial Design: 6 Models

### Model 1: Baseline
**File**: `model1_baseline.stan`
**Parameters**: `alpha`, `beta`

- No modulation by salience
- No perseveration
- Simplest model - baseline for comparison

---

### Model 2: Alpha Shift Only
**File**: `model2_alpha_shift.stan`
**Parameters**: `alpha`, `beta`, `alpha_shift`

- Tests if **salient feedback modulates learning rate**
- When `salient_feedback == 1`: effective_alpha = `alpha + alpha_shift`
- When `salient_feedback == 0`: effective_alpha = `alpha`
- No perseveration

---

### Model 3: Kappa Only
**File**: `model3_kappa.stan`
**Parameters**: `alpha`, `beta`, `kappa`

- Tests for **choice perseveration/stickiness**
- Constant perseveration (not modulated by salience)
- Perseveration adds bonus to previously chosen option
- No learning rate modulation

---

### Model 4: Alpha Shift + Kappa
**File**: `model4_alpha_shift_kappa.stan`
**Parameters**: `alpha`, `beta`, `alpha_shift`, `kappa`

- Tests **both effects independently** (no interaction)
- Salience modulates learning rate via `alpha_shift`
- Constant perseveration via `kappa` (not modulated)
- Tests if both mechanisms operate simultaneously

---

### Model 5: Kappa + Kappa Shift
**File**: `model5_kappa_kappa_shift.stan`
**Parameters**: `alpha`, `beta`, `kappa`, `kappa_shift`

- Tests if **salient feedback modulates perseveration**
- When previous trial had `salient_feedback == 1`: effective_kappa = `kappa + kappa_shift`
- When previous trial had `salient_feedback == 0`: effective_kappa = `kappa`
- No learning rate modulation
- Tests if salient wins make people stickier

---

### Model 6: Full Model
**File**: `model6_full.stan`
**Parameters**: `alpha`, `beta`, `alpha_shift`, `kappa`, `kappa_shift`

- **Complete model** with all parameters
- Salience modulates learning rate (`alpha_shift`)
- Salience modulates perseveration (`kappa_shift`)
- Tests full hypothesis: salient cues affect both learning AND choice stickiness

---

## Key Implementation Details

### Alpha Shift
- Applied to **current trial's learning update**
- Modulated by **current trial's feedback salience**
- Formula: `effective_alpha = Phi(alpha_raw + alpha_shift_raw)` when salient

### Kappa Shift
- Applied to **choice policy** (perseveration bonus)
- Modulated by **previous trial's feedback salience**
- Formula: `effective_kappa = Phi(kappa_raw + kappa_shift_raw)` when previous was salient
- Adds bonus to previously chosen option

### Phi Transformations
- `alpha`: `Phi(alpha_raw)` → [0, 1]
- `beta`: `Phi(beta_raw) * 10` → [0, 10]
- `kappa`: `Phi(kappa_raw)` → [0, 1]

## Model Comparison Strategy

1. **Fit all 6 models** using Stan
2. **Compare** using LOO-IC or WAIC
3. **Theoretical questions**:
   - Does salience affect learning? (Model 2 vs Model 1)
   - Is there perseveration? (Model 3 vs Model 1)
   - Does salience affect perseveration? (Model 5 vs Model 3)
   - Are both mechanisms present? (Model 4, 6 vs others)
   - Is there an interaction? (Model 6 vs Model 4)

## Next Steps

1. ☐ Test compilation of all models
2. ☐ Fit all 6 models with proper settings:
   - Increased warmup iterations (suggested by professor for uniform priors)
   - Sensible initial values (e.g., alpha=0.5, beta=0.5 in transformed space)
3. ☐ Check convergence diagnostics (R-hat < 1.1, sufficient n_eff)
4. ☐ Compare models using LOO
5. ☐ Examine parameter estimates and posteriors
6. ☐ Validate with professor

## Notes

- All models use **centered parameterization** (CP)
- If convergence issues arise with uniform priors, may need to increase warmup
- Professor suggested sensible initial values for group-level means
- Watch for divergent transitions and adjust `adapt_delta` if needed

## References

- Slack discussion: 2025-04-28 to 2025-05-23
- Professor Jan's feedback on priors and transformations
- Based on original models: `rl_cp_basic_uniform.stan`, `rl_cp_shift_persev_uniform.stan`
