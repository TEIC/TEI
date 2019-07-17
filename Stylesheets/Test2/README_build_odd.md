# Documentation for the build_odd.xml Ant file

This file documents the components and functionality of the build_odd.xml ant file, which handles testing of ODD processing.

## Invocation
build_odd.xml is a separate file in order to provide some modularization and keep file sizes under control, but it should be invoked using the main ant file, build.xml:

`ant odd`

## ODD to RELAX NG tests

Most of the work done so far is on converting ODD to RELAX NG. The basic process works like this:

In the `build_odd.xml` ant file there is a property containing a list of files to be processed:

```
<property name="oddFileList" value="testSpecificationDescription.odd testPure1.odd testNonTeiOdd1.odd"/>
```

Those files (for example `testPure1.odd`) exist in the inputFiles folder. All the files in that list are processed by default. Each file goes through the following process (using the example of `testPure1.odd`):

- Conversion to RELAX NG (using `bin/teitoRELAX NG`). This creates the result `outputFiles/testPure1.rng`.
- Preparation of the result for diffing against expected results. This runs a range of normalization processes against the generated RELAX NG, to remove any components of the output which are processor- or occasion-dependent.
- Diffing against expected results. The expected results file is `expected-results/testPure1.rng`.
- Extraction of Schematron from the ODD file. This creates a file called `outputFiles/testPure1FromOdd.sch`.

## Testing of the RELAX NG

Generated RELAX NG files are not simply diffed against expected results; they're also tested against two TEI instance files, one of which is intended to be valid, and one of which is intended to be invalid in specific ways. These two files are named according to a convention based on the original ODD file name:

- `inputFiles/validInstances/testPure1ValidInstance.xml`
- `inputFiles/invalidInstances/testPure1InvalidInstance.xml`

Note that for a single input ODD file, we expect at most a single valid instance and a single invalid instance file. Every test ODD should have well-documented components which match up to components in the two instance files, so that features in the ODD are tested in useful ways.

If there are no such files, then nothing happens during this stage, but if instance files are found, they are processed in the following way:

- The valid instance file is validated with the newly-generated RELAX NG file (`outputFiles/testPure1.rng`). If this validation fails, the process stops with an error.

- The invalid instance file is validated against the RELAX NG file. The error messages resulting from this validation are stored in a text file in the `outputFiles` folder, again named according to convention: `testNonTeiOdd1InvalidInstanceRngMessages.txt`.
- The error message file is diffed against a file with the same name in the `expected-results` folder. If the files are different, the process stops with an error.

## How to add new tests

Imagine that you want to add a new test to the `testPure1.odd` file. We recommend the following procedure:

 - Add only one new test/feature at a time.
 - Think clearly about what you want to test and how you want to test it.
 - Add a well-commented block to the `testPure1.odd` file which invokes the component of the transformations that you want to test.
 - Add a new section (with explanatory comments) to the `inputFiles/validInstances/testPure1ValidInstance.xml` file which comprises a structure which should be valid given the new ODD component in `testPure1.odd`.
 - Add a new section (with comments) to the `inputFiles/invalidInstances/testPure1InvalidInstance.xml` file which should be invalid in a specific way dependent on the section you have added to `testPure1.odd`.
 - Run the tests. The first thing that will fail is the that the generated ODD file in `outputFiles/testPure1.rng` will no longer match the version in `expected-results`.  Open these two files in a diff tool; check that the differences in the new version are what should be expected from your changes, and if so, copy the version in `outputFiles/testPure1.rng` over the version in `expected-results`.
 - Run the tests again. The RELAX NG file should now pass. If everything is working as expected, the *valid instance* file should also pass; if not, check into that problem and fix it.
 - Finally, we would expect the *invalid instance* file to be invalid as before, but it will now generate additional error messages based on the new changes you made. Since the *invalid instance* test is based on diffing the current validation failure error messages against the expected error messages, the test will fail because the messages are now slightly different. Diff the error message files (in our example case, `outputFiles/testPure1InvalidInstanceRngMessages.txt` and `expected-results/testPure1InvalidInstanceRngMessages.txt`) and check that the differences are what you would expect to see. If so, copy the former over the latter and save.
 - Run the tests again. Everything should now complete successfully. 
 - Commit and push your changes.


