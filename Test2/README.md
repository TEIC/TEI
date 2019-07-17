# Stylesheets Test2 Project

Test2 is a long-term project to replace the existing Test suite with a more comprehensible, better documented test suite that will run faster and be easier to modify and update.

Rather than Make it uses only Ant, and for the sake of speed, under normal conditions many of the tests are run in parallel, using more computing resources but running faster. Input, output and expected-results materials are also (hopefully) better organized, segregated, and named than in the original tests. 

For a detailed introduction to how it works, run "ant -projecthelp" in the Test2 directory.

Martin  Holmes (@martindholmes) and Syd Bauman (@sydb) are currently doing the work on this, but feel free to join the effort! Current work is focusing on ODD processing, in build_odd.xml. 


Example usages:

`ant test` runs all the tests.

`ant clean` removes results from previous runs of the tests.

`ant odt` runs only the odt tests. Similarly, `ant docx`, `ant fo`, `ant odd`, and others.





