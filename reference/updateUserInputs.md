# Update User Inputs

Update values from 'input' with all values from userInputs. Entries that
are NULL are removed. Only updates entries that are present in 'input'.

## Usage

``` r
updateUserInputs(input, output, session, userInputs)
```

## Arguments

- input:

  input object from server function

- output:

  output object from server function

- session:

  session from server function

- userInputs:

  (list) named list of inputs to be updated
