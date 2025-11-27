# Summary: 6-Model Factorial Design

## What We Built

Based on your German notes and professor's feedback, we created **6 Stan models** with a factorial design testing how salient feedback affects:
- Learning rate (`alpha_shift`)
- Perseveration/stickiness (`kappa_shift`)

## Key Changes from Old Models

### 1. Prior Specifications ⚠️ UPDATED
```stan
// OLD:
alpha_mu_raw ~ uniform(-4, 4);
beta_mu_raw ~ uniform(-4, 4);
alpha_sd_raw ~ uniform(0.001, 3);

// NEW:
alpha_mu_raw ~ uniform(-3, 3);        // Narrower range
beta_mu_raw ~ uniform(-3, 3);         // Narrower range
alpha_sd_raw ~ uniform(0.0001, 10);   // Much wider upper bound
```

### 2. Kappa Transformation ⚠️ NEW
```stan
// Kappa is now Phi-transformed (like alpha)
kappa_subj_transformed[subi] = Phi(kappa_subj_raw[subi]);  // → [0, 1]
```

### 3. Kappa Shift Implementation ⚠️ NEW PARAMETER
```stan
// Kappa modulated by PREVIOUS trial's salience
if (prev_salient_feedback == 1) {
  effective_kappa = kappa + kappa_shift;  // More/less sticky after salient feedback
}
```

## The 6 Models

| Model | File | Parameters | Tests |
|-------|------|------------|-------|
| **1** | `model1_baseline.stan` | α, β | Baseline (no effects) |
| **2** | `model2_alpha_shift.stan` | α, β, α_shift | Salience → learning rate |
| **3** | `model3_kappa.stan` | α, β, κ | Perseveration only |
| **4** | `model4_alpha_shift_kappa.stan` | α, β, α_shift, κ | Both (no interaction) |
| **5** | `model5_kappa_kappa_shift.stan` | α, β, κ, κ_shift | Salience → perseveration |
| **6** | `model6_full.stan` | α, β, α_shift, κ, κ_shift | Full model (all effects) |

## Theoretical Questions Each Model Answers

### Model 2 vs 1
❓ Does salient feedback make people learn faster/slower?

### Model 3 vs 1
❓ Do people show choice perseveration (repeat previous choice)?

### Model 5 vs 3
❓ Does salient feedback make people more/less sticky?

### Model 4 vs 2+3
❓ Do learning rate shifts and perseveration operate independently?

### Model 6 vs 4
❓ Is there an interaction? (Does salience modulate BOTH α and κ?)

## What Your German Notes Meant

```
Lernraten-Shift + Perseveration
```
→ We need BOTH alpha_shift AND kappa (with potential kappa_shift)

```
alpha mu, beta mu → uniform(-3,3) → nicht -4,4
```
→ Changed prior range from (-4,4) to (-3,3) ✓

```
sigma priors müssen immer uniform priors z.B. 0,0001-10
```
→ All SD priors now uniform(0.0001, 10) ✓

```
alpha shift kann normal 0,1 bleiben
```
→ Kept alpha_shift_mu_raw ~ normal(0, 1) ✓

```
kappa phi transformiert, normal prior für kappa
```
→ Kappa: Phi-transformed + normal(0,1) prior ✓

```
Beta Mu Multiplikation relaxen (z.B. 10 statt 4)
```
→ Already had: `Phi(beta) * 10` ✓

## Next Steps

1. **Test compilation** of all models
2. **Fit all 6 models** with:
   - More warmup iterations (uniform priors may need more)
   - Sensible starting values
3. **Check convergence** (R-hat, n_eff, divergent transitions)
4. **Model comparison** (LOO-IC)
5. **Verify with professor** before proceeding

## Files Created

```
analysis/models/new_models/
├── model1_baseline.stan
├── model2_alpha_shift.stan
├── model3_kappa.stan
├── model4_alpha_shift_kappa.stan
├── model5_kappa_kappa_shift.stan
├── model6_full.stan
├── README.md          # Detailed documentation
└── SUMMARY.md         # This file
```

## Important Notes

⚠️ **Kappa vs Alpha shift timing**:
- `alpha_shift`: Uses **current trial's** salience (for learning update)
- `kappa_shift`: Uses **previous trial's** salience (for choice stickiness)

⚠️ **All models use Centered Parameterization (CP)**:
- If you get convergence issues, may need Non-Centered (NCP) versions
- Or increase warmup/adapt_delta

⚠️ **Double-check with your professor**:
- Confirm these are the 6 models he intended
- Verify kappa_shift timing (previous vs current trial salience)
