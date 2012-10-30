#!/bin/bash
#The Mighty Jenkins Builder Script.

# This script is designed to set up a Jenkins Continuous
# Integration server which will build all of the TEI 
# products automatically from the TEI SVN repository on
# SourceForge. 

# For this to work, you will need a valid license for 
# Oxygen, which is required for building some of the products.

# To use this script, first set up an Ubuntu 12.04 server
# with default configuration (no need to install anything 
# in particular). 

# Next, log into the server and create the directory 
# /usr/share/oxygen, then put a file named licensekey.txt 
# with the nine lines of text of the Oxygen license key 
# (located between the license key start and end markers)
# into that directory.

# Then you can put this script on the server and run it 
# as root to create the server build.

#Note that this should be run as root (with sudo).

#Required location of Oxygen licence.
OxyLicense="/usr/share/oxygen/licensekey.txt"

echo ""
echo "*******************************************"
echo "The purpose of this script is to set up a working
Jenkins Continuous Integration Server which will check
out and build a range of TEI products, including the 
P5 Guidelines (in various formats) and the Roma schema 
generation tool."
echo ""
echo "This script is designed to be run on a fully-updated
install of Ubuntu Precise Pangolin (Ubuntu 12.04). Precise was 
chosen because it is a Long-Term Support edition, and 
will be available for around two years from the time 
of writing the script."
echo ""
echo "The script may work on other versions of Ubuntu,
but only Precise has been tested."
echo "*******************************************"
echo "Press return to continue"
read
echo ""
echo "*******************************************"
echo "In order for Jenkins to build the TEI packages, you will
need to have a registration key for the Oxygen XML Editor. "
echo ""
echo "You must provide a license for Oxygen, in the form of a 
file named licensekey.txt with the nine lines of text of the 
license key (located between the license key start and end 
markers). "
echo ""
echo "This should be placed in /usr/share/oxygen. Create that 
directory if it does not exist."
echo "This script will check for the existence of that file, and
terminate if it does not exist."
echo "*******************************************"
echo ""
echo "Do you want to continue? Press return to continue,
Control+c to stop."
read


#Save the current directory so that we can come back here.
currDir=`pwd`

echo ""
echo "Entering the Mighty Jenkins Builder Script."

uid=$(/usr/bin/id -u) && [ "$uid" = "0" ] ||
{ echo "This script must be run as root."; exit 1; }

echo "Running as root: good."
echo ""

if grep -q 12.04 /etc/lsb-release 
then echo "Running on Ubuntu Precise. Good."
else
  echo "This script needs to be run on Ubuntu Precise Server."
  echo "According to /etc/lsb-release, you don't seem to be 
running that version of Ubuntu."
  echo "The script will now terminate."
  exit
fi
echo ""
#Check for existence of an Oxygen licence key file in /usr/share/oxygen

if [ -f $OxyLicense ];
then echo "Oxygen license is in the right place."
else
  echo "You must provide a license for Oxygen, in the form of a 
file named licensekey.txt containing the nine lines of text of 
the license key (located between the license key start and end 
markers). "
  echo "This should be placed in /usr/share/oxygen. Create that 
directory if it does not exist."
  echo "The script will now terminate. Run it again when you 
have installed the Oxygen license key."
  exit
fi

echo ""
echo "Using netstat to check whether any service is currently 
running on port 8080."
echo ""
netstat -tulpan | grep 8080
if [ $? -eq 0 ] 
then echo "Another service appears to be running on port 8080, 
which is the default port for Jenkins."
  echo "You can either continue, and then change the port on 
which Jenkins runs later, or stop now, and move that service 
to another port."
echo ""
  echo "Press return to continue, or Control+c to stop."
  read
fi


#Set -e so we exit if something goes wrong. More useful error messages
#would be helpful, in the future.
set -e

echo ""
echo "*******************************************"
echo "The following build process should proceed without
any need for intervention from you, but if it fails at
some point, you can start it again, or read through the 
script and run each section separately to figure out what
the problem is."
echo "*******************************************"
echo ""

echo "Press return to continue"
read

#We used to start by installing the MS fonts, which have EULAs, so if we got that 
#bit out of the way, the rest of the install could proceed basically unattended. But
#switching from Times New Roman to Libertine should remove that requirement.
#echo "We'll start by installing some fonts we need. 
#You'll have to agree to a EULA here."
#apt-get -y install msttcorefonts

#Now do updates.
echo "Doing system updates before starting on anything else."
apt-get update
apt-get -y upgrade

echo "Installing required fonts."
apt-get -y install ttf-dejavu ttf-arphic-ukai ttf-arphic-uming ttf-baekmuk ttf-junicode ttf-kochi-gothic ttf-kochi-mincho ttf-linux-libertine
echo ""

#Now add the repositories we want.
echo "Backing up repository list."
cp /etc/apt/sources.list /etc/apt/sources.list.bak
echo ""

#Uncomment partner repos.
echo "Uncommenting partner repositories on sources list."
sed -i -re '/partner/ s/^#//' /etc/apt/sources.list
echo ""

#First Jenkins
echo "Adding Jenkins repository."
wget -q -O - http://pkg.jenkins-ci.org/debian/jenkins-ci.org.key | apt-key add -
echo "deb http://pkg.jenkins-ci.org/debian binary/" > /etc/apt/sources.list.d/jenkins.list
echo ""

#Next TEI. Allow a 5-minute timeout for this one; it's insanely slow.
echo "Adding TEI Debian repository. It may take some time 
to retrieve the key."
gpg --keyserver wwwkeys.pgp.net --keyserver-options timeout=300 --recv-keys FEA4973F86A9A497
apt-key add ~/.gnupg/pubring.gpg
echo "deb http://tei.oucs.ox.ac.uk/teideb/binary ./" > /etc/apt/sources.list.d/tei.list
echo ""

#Now we can start installing packages.
echo "Updating for new repositories."
apt-get update
echo ""

#We will need a JDK, so we try to install the one to match the default OpenJDK JRE that's installed.
echo "Installing the OpenJDK Java Development Kit."
apt-get -y install openjdk-6-jdk
echo ""

#We need Maven for the OxGarage install.
echo "Installing the Maven project tool"
apt-get -y install maven2
echo ""

echo "Installing core packages we need."
apt-get -y install openssh-server libxml2 libxml2-utils devscripts xsltproc debhelper subversion trang zip &&
echo "Installing curl, required for some tei building stuff."
apt-get -y install curl &&
echo ""

#TEI packages
echo "Installing TEI packages."
apt-get -y --force-yes install psgml xmlstarlet debiandoc-sgml linuxdoc-tools jing jing-trang-doc libjing-java texlive-xetex &&
apt-get -y --force-yes install trang-java saxon onvdl tei-p5-exemplars tei-p5-xsl2 tei-p5-xslprofiles tei-roma tei-p5-doc tei-xsl-common tei-p5-source tei-p5-schema tei-oxygen zip &&
echo ""

echo "The Han Nom font is not available in repositories, 
so we have to download it from SourceForge."
cd /usr/share/fonts/truetype
mkdir hannom
cd hannom
wget -O hannom.zip http://downloads.sourceforge.net/project/vietunicode/hannom/hannom%20v2005/hannomH.zip
if [ $? != 0 ]; then
{
    echo "Failed to download Hannom font from SourceForge."
    echo "This is not crucial, but if you want to make sure it
is installed, press Control+C to exit now, and run this 
script again. Otherwise, press return to continue."
    read
} fi
unzip hannom.zip
find . -iname "*.ttf" | rename 's/\ /_/g'
rm hannom.zip
fc-cache -f -v

#Go back to our home directory.
cd $currDir
echo "Changed back to $currDir"

#Downloading and installing rnv
echo "Downloading and building rnv (the RelaxNG validator) from SourceForge."
echo "First we need libexpat-dev, on which it depends."
apt-get -y install libexpat-dev
echo ""
echo "Now we download rnv, build and install it."
#This seems to be fragile. Let's catch it in case it fails. Lots of apparently good URLs fail when 
#using wget, so I've fallen back on using curl -L. We'll see how reliable this is.
#wget -O rnv-1.7.10.zip http://sourceforge.net/projects/rnv/files/Sources/1.7.10/rnv-1.7.10.zip/download
#wget -O rnv-1.7.10.zip http://downloads.sourceforge.net/projects/rnv/Sources/1.7.10/rnv-1.7.10.zip?r=\&ts=1338494052\&use_mirror=iweb
#wget -O rnv-1.7.10.zip http://sourceforge.net/projects/rnv/files/Sources/1.7.10/rnv-1.7.10.zip/download?use_mirror=voxel
curl -L http://sourceforge.net/projects/rnv/files/Sources/1.7.10/rnv-1.7.10.zip/download > rnv-1.7.10.zip
if [ $? != 0 ]; then
{
    echo "Failed to download rnv source code from SourceForge."
    echo "This is not crucial, but if you want to make sure rnv
is installed, press Control+C to exit now, and run this 
script again. Otherwise, press return to continue."
    read
} fi
unzip rnv-1.7.10.zip
if [ $? != 0 ]; then
{
    echo "Failed to unzip the rnv source code from SourceForge."
    echo "This is not crucial, but if you want to make sure rnv
is installed, press Control+C to exit now, and run this 
script again. Otherwise, press return to continue."
    read
} fi
cd rnv-1.7.10
./configure
make
make install
if [ $? != 0 ]; then
{
    echo "Failed to build and install rnv from SourceForge."
    echo "This is not crucial, but if you want to make sure rnv
is installed, press Control+C to exit now, and run this 
script again. Otherwise, press return to continue."
    read
} fi

echo ""

#Setting up configuration for oXygen
#This particular line is very unfortunate, but we apparently have to do it.
chmod a+x /root
mkdir /root/.com.oxygenxml
chmod a+x /root/.com.oxygenxml
mkdir /root/.java
chmod a+x /root/.java
touch  /root/.java/.com.oxygenxml.rk
chmod a+w /root/.java/.com.oxygenxml.rk

#Jenkins
echo "Installing the Jenkins CI Server."
apt-get -y install jenkins
echo ""

#Configuration for Jenkins
echo "Starting configuration of Jenkins."
echo "Getting the Hudson log parsing rules from TEI SVN."
cd /var/lib/jenkins
svn export https://tei.svn.sourceforge.net/svnroot/tei/trunk/Documents/Editing/Jenkins/hudson-log-parse-rules
chown jenkins hudson-log-parse-rules
svn export https://tei.svn.sourceforge.net/svnroot/tei/trunk/Documents/Editing/Jenkins/hudson.plugins.logparser.LogParserPublisher.xml
chown jenkins hudson.plugins.logparser.LogParserPublisher.xml
echo ""

echo "Getting all the job data from TEI SVN."
#Don't bring down the config.xml file for now; that contains security settings specific to 
#Sebastian's setup, and will prevent anyone from logging in. We leave the server unsecured,
#and make it up to the user to secure it.
#svn export https://tei.svn.sourceforge.net/svnroot/tei/trunk/Documents/Editing/Jenkins/config.xml
#chown jenkins config.xml
svn export --force https://tei.svn.sourceforge.net/svnroot/tei/trunk/Documents/Editing/Jenkins/jobs/ jobs
chown -R jenkins jobs
echo ""

echo "Installing Jenkins plugins."
cd plugins
wget --no-check-certificate http://updates.jenkins-ci.org/latest/copyartifact.hpi
chown jenkins copyartifact.hpi
wget --no-check-certificate http://updates.jenkins-ci.org/latest/emotional-hudson.hpi
chown jenkins emotional-hudson.hpi
wget --no-check-certificate http://updates.jenkins-ci.org/latest/greenballs.hpi
chown jenkins greenballs.hpi
wget --no-check-certificate http://updates.jenkins-ci.org/latest/jobConfigHistory.hpi
chown jenkins jobConfigHistory.hpi
wget --no-check-certificate http://updates.jenkins-ci.org/latest/plot.hpi
chown jenkins plot.hpi
wget --no-check-certificate http://updates.jenkins-ci.org/latest/log-parser.hpi
chown jenkins log-parser.hpi
wget --no-check-certificate http://updates.jenkins-ci.org/latest/scp.hpi
chown jenkins scp.hpi
wget --no-check-certificate http://updates.jenkins-ci.org/latest/WebSVN2.hpi
chown jenkins WebSVN2.hpi
wget --no-check-certificate http://updates.jenkins-ci.org/latest/PrioritySorter.hpi
chown jenkins PrioritySorter.hpi
echo ""

#Now we need to find out what the Jenkins version is, and stash the result in a variable for later use.
echo "Discovering Jenkins version..."
cd /tmp
wget http://localhost:8080/jnlpJars/jenkins-cli.jar
JINKSVERSION=`java -jar jenkins-cli.jar -s http://localhost:8080 version`

echo "Stopping Jenkins server, so that we can reconfigure all the jobs a little."
/etc/init.d/jenkins stop
echo ""

#Reconfigure Jinks jobs with user's email, and adding priority settings if necessary.
#NOTE: Avoiding this, because you need to set up a whole host of Jenkins config files
#in order to make emailing work.
#echo "If you want Jenkins to notify you when a build fails, please enter your email address now:"
#read email

echo "Downloading various configuration files for Jenkins."
cd /var/lib/jenkins
svn export https://tei.svn.sourceforge.net/svnroot/tei/trunk/Documents/Editing/Jenkins/jenkins_job_config.xsl
chown jenkins jenkins_job_config.xsl
svn export https://tei.svn.sourceforge.net/svnroot/tei/trunk/Documents/Editing/Jenkins/jenkins_main_config.xsl
chown jenkins jenkins_main_config.xsl
svn export https://tei.svn.sourceforge.net/svnroot/tei/trunk/Documents/Editing/Jenkins/defaultConfig.xml
mv defaultConfig.xml config.xml
saxon -s:/var/lib/jenkins/config.xml -xsl:/var/lib/jenkins/jenkins_main_config.xsl -o:/var/lib/jenkins/config.xml jinksVersion=$JINKSVERSION
chown jenkins config.xml
echo "Downloaded and set up root configuration file."

echo "Configuring job priorities settings."

echo "Running transformations on job configurations."
saxon -s:/var/lib/jenkins/jobs/OxGarage/config.xml -xsl:/var/lib/jenkins/jenkins_job_config.xsl -o:/var/lib/jenkins/jobs/OxGarage/config.xml jobPriority=90 email=
saxon -s:/var/lib/jenkins/jobs/Roma/config.xml -xsl:/var/lib/jenkins/jenkins_job_config.xsl -o:/var/lib/jenkins/jobs/Roma/config.xml jobPriority=90 email=
saxon -s:/var/lib/jenkins/jobs/Stylesheets/config.xml -xsl:/var/lib/jenkins/jenkins_job_config.xsl -o:/var/lib/jenkins/jobs/Stylesheets/config.xml jobPriority=100 email=
saxon -s:/var/lib/jenkins/jobs/Stylesheets1/config.xml -xsl:/var/lib/jenkins/jenkins_job_config.xsl -o:/var/lib/jenkins/jobs/Stylesheets1/config.xml jobPriority=90 email=
saxon -s:/var/lib/jenkins/jobs/TEIP5/config.xml -xsl:/var/lib/jenkins/jenkins_job_config.xsl -o:/var/lib/jenkins/jobs/TEIP5/config.xml jobPriority=10 email=
saxon -s:/var/lib/jenkins/jobs/TEIP5-Documentation/config.xml -xsl:/var/lib/jenkins/jenkins_job_config.xsl -o:/var/lib/jenkins/jobs/TEIP5-Documentation/config.xml jobPriority=10 email=
saxon -s:/var/lib/jenkins/jobs/TEIP5-Test/config.xml -xsl:/var/lib/jenkins/jenkins_job_config.xsl -o:/var/lib/jenkins/jobs/TEIP5-Test/config.xml jobPriority=10 email=
echo ""

echo "Starting the Jenkins server."
/etc/init.d/jenkins start

echo "Now we want to trigger the server to save its configuration before restarting it."
#Sleep while waiting for Jinks to come back up.
sleep 10
#Now we try making Jinks save its configuration. This appallingly messy line of code comes from 
#using Firefox's Live HTTP Headers extension to figure out what happens when you save the 
#config. If we don't do this, it seems that Jenkins will never figure out that it needs to use the 
#hudson-log-parse-rules file to parse build logs, which means that many errors which aren't really
#errors will appear.
echo "Trying to force Jenkins to save its configuration using curl."
curl -d "_.rawWorkspaceDir=%24%7BITEM_ROOTDIR%7D%2Fworkspace&_.rawBuildsDir=%24%7BITEM_ROOTDIR%7D%2Fbuilds&system_message=&_.numExecutors=2&_.quietPeriod=5&_.scmCheckoutRetryCount=0&namingStrategy=0&stapler-class=jenkins.model.ProjectNamingStrategy%24DefaultProjectNamingStrategy&stapler-class=jenkins.model.ProjectNamingStrategy%24PatternProjectNamingStrategy&_.namePattern=.*&slaveAgentPort.type=random&stapler-class=hudson.markup.RawHtmlMarkupFormatter&stapler-class=hudson.security.LegacySecurityRealm&stapler-class=hudson.security.HudsonPrivateSecurityRealm&privateRealm.allowsSignup=on&stapler-class=hudson.security.LDAPSecurityRealm&ldap.server=&ldap.rootDN=&ldap.userSearchBase=&ldap.userSearch=&ldap.groupSearchBase=&ldap.managerDN=&ldap.managerPassword=&stapler-class=hudson.security.PAMSecurityRealm&_.serviceName=&authorization=0&stapler-class=hudson.security.AuthorizationStrategy%24Unsecured&stapler-class=hudson.security.LegacyAuthorizationStrategy&stapler-class=hudson.security.FullControlOnceLoggedInAuthorizationStrategy&stapler-class=hudson.security.GlobalMatrixAuthorizationStrategy&stapler-class=hudson.security.ProjectMatrixAuthorizationStrategy&stapler-class=hudson.security.csrf.DefaultCrumbIssuer&name=jobConfigHistory&historyRootDir=&maxHistoryEntries=&excludePattern=queue%7CnodeMonitors%7CUpdateCenter%7Cglobal-build-stats&globalMavenOpts=&stapler-class=hudson.maven.local_repo.DefaultLocalRepositoryLocator&stapler-class=hudson.maven.local_repo.PerExecutorLocalRepositoryLocator&stapler-class=hudson.maven.local_repo.PerJobLocalRepositoryLocator&_.usageStatisticsCollected=on&port.type=random&_.cvsExe=&_.cvspassFile=&svn.workspaceFormat=8&svn.global_excluded_revprop=&svn.storeAuthToDisk=on&shell=&_.url=http%3A%2F%2Flocalhost%3A7070%2F&_.smtpServer=&_.defaultSuffix=&_.adminAddress=address+not+configured+yet+%3Cnobody%40nowhere%3E&_.smtpAuthUserName=&_.smtpAuthPassword=&_.smtpPort=&_.replyToAddress=&_.charset=UTF-8&sendTestMailTo=&log-parser.name=TEI+Log+Parse+Rules&log-parser.path=%2Fvar%2Flib%2Fjenkins%2Fhudson-log-parse-rules&log-parser.name=&log-parser.path=&core%3Aapply=&json=%7B%22rawWorkspaceDir%22%3A+%22%24%7BITEM_ROOTDIR%7D%2Fworkspace%22%2C+%22rawBuildsDir%22%3A+%22%24%7BITEM_ROOTDIR%7D%2Fbuilds%22%2C+%22system_message%22%3A+%22%22%2C+%22%22%3A+%22%22%2C+%22jenkins-model-MasterBuildConfiguration%22%3A+%7B%22numExecutors%22%3A+%222%22%7D%2C+%22jenkins-model-GlobalQuietPeriodConfiguration%22%3A+%7B%22quietPeriod%22%3A+%225%22%7D%2C+%22jenkins-model-GlobalSCMRetryCountConfiguration%22%3A+%7B%22scmCheckoutRetryCount%22%3A+%220%22%7D%2C+%22jenkins-model-GlobalProjectNamingStrategyConfiguration%22%3A+%7B%7D%2C+%22hudson-security-GlobalSecurityConfiguration%22%3A+%7B%7D%2C+%22hudson-security-csrf-GlobalCrumbIssuerConfiguration%22%3A+%7B%7D%2C+%22jenkins-model-GlobalNodePropertiesConfiguration%22%3A+%7B%22globalNodeProperties%22%3A+%7B%7D%7D%2C+%22jenkins-model-GlobalPluginConfiguration%22%3A+%7B%22plugin%22%3A+%7B%22name%22%3A+%22jobConfigHistory%22%2C+%22historyRootDir%22%3A+%22%22%2C+%22maxHistoryEntries%22%3A+%22%22%2C+%22saveSystemConfiguration%22%3A+false%2C+%22excludePattern%22%3A+%22queue%7CnodeMonitors%7CUpdateCenter%7Cglobal-build-stats%22%2C+%22skipDuplicateHistory%22%3A+false%7D%7D%2C+%22hudson-maven-MavenModuleSet%22%3A+%7B%22globalMavenOpts%22%3A+%22%22%2C+%22%22%3A+%220%22%2C+%22localRepository%22%3A+%7B%22stapler-class%22%3A+%22hudson.maven.local_repo.DefaultLocalRepositoryLocator%22%7D%7D%2C+%22hudson-model-UsageStatistics%22%3A+%7B%22usageStatisticsCollected%22%3A+%7B%7D%7D%2C+%22org-jenkinsci-main-modules-sshd-SSHD%22%3A+%7B%22port%22%3A+%7B%22value%22%3A+%22%22%2C+%22type%22%3A+%22random%22%7D%7D%2C+%22hudson-scm-CVSSCM%22%3A+%7B%22cvsExe%22%3A+%22%22%2C+%22cvspassFile%22%3A+%22%22%2C+%22cvs_noCompression%22%3A+false%7D%2C+%22hudson-scm-SubversionSCM%22%3A+%7B%22workspaceFormat%22%3A+%228%22%2C+%22global_excluded_revprop%22%3A+%22%22%2C+%22storeAuthToDisk%22%3A+%7B%7D%7D%2C+%22hudson-tasks-Shell%22%3A+%7B%22shell%22%3A+%22%22%7D%2C+%22hudson-tasks-Mailer%22%3A+%7B%22url%22%3A+%22http%3A%2F%2Flocalhost%3A7070%2F%22%2C+%22smtpServer%22%3A+%22%22%2C+%22defaultSuffix%22%3A+%22%22%2C+%22adminAddress%22%3A+%22address+not+configured+yet+%3Cnobody%40nowhere%3E%22%2C+%22useSsl%22%3A+false%2C+%22smtpPort%22%3A+%22%22%2C+%22replyToAddress%22%3A+%22%22%2C+%22charset%22%3A+%22UTF-8%22%7D%2C+%22hudson-plugins-logparser-LogParserPublisher%22%3A+%7B%22rule%22%3A+%5B%7B%22name%22%3A+%22TEI+Log+Parse+Rules%22%2C+%22path%22%3A+%22%2Fvar%2Flib%2Fjenkins%2Fhudson-log-parse-rules%22%7D%2C+%7B%22name%22%3A+%22%22%2C+%22path%22%3A+%22%22%7D%5D%7D%2C+%22core%3Aapply%22%3A+%22%22%7D&Submit=Save" http://localhost:8080/configSubmit

echo "Waiting to allow Jenkins server to store configuration before restarting the server."
sleep 10
echo "Restarting Jenkins server."
/etc/init.d/jenkins restart

#NOTE: No need for the lines below because the Priority Sorter plugin should handle it.
#echo "Triggering the Stylesheet job. It needs to be completed before other P5 builds will succeed."
#wget http://localhost:8080/job/Stylesheets/build > /dev/null
#echo "Stylesheets build has been triggered."

echo ""
echo "*******************************************"
echo "OK, we should be done. Now you have to:"
echo "Go to the Jenkins interface on 
http://[this_computer_ip]:8080, and set up 
authentication. Read the Jenkins documentation 
for help with this."
echo ""
echo "Make sure you do this, because your Jenkins 
installation is currently unsecured, and anyone 
could make changes to it."
echo ""
echo "If some builds fail initially, it may be simply 
due to sequencing and timing. The Stylesheets build
must successfully complete before any of the TEIP5 
builds will be able to work."
echo ""
echo "That's it!"
echo "Press return to exit."
echo "*******************************************"
read
exit


