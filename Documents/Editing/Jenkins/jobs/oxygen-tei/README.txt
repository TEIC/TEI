This job requires a recent version of oxygen.jar. The 
build script is set up so that it looks for one in the 
job folder (oxygen-tei), and if it finds one, copies 
it into the workspace/oxygen-tei/lib folder before 
trying to build. If it does not find one, it will fail.

It's set up this way because if the SVN repo gets wiped 
out and restored, the lib folder will be nuked in the 
process, so it's dangerous to store it there by default.
