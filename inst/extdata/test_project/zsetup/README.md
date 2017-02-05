## Variants included in the basic tutorial

1. 01_core_area_zonation
1. 02_additive_benefit_function
1. 03_boundary_length_penalty
1. 04_distribution_smoothing
1. 05_hierarchical_removal_mask
1. 06_dummy_for_testing

### 01: Core-area Zonation

Basic core-area Zonation run.

### 02: Additive benefit function

Basic additive benefit function run with weights.

### 03: Boundary length penalty

Basic core-area Zonation run using boundary length penalty (BLP) to account for connectivity.

### 04: Distribution smoothing

Basic core-area Zonation run with weights using distribution smoothing (DS) to account for connectivity.

### 05: Hierarchical removal mask

Basic core-area Zonation run with weights using distribution smoothing (DS) to account for connectivity. 
Additionally, a fictional reserve network mask is used for hierarchical masking in order to find which
additional areas would be the most suitable for reserve network expansion.

### 06: Dummy for testing

This is a copy of variant 05 **only meant to provide a test case** as variant which hasn't been run yet. 
The associated bat-file is named whit an extension batx so that it won't be run accidentally (e.g. if 
looping over all the bat-files).