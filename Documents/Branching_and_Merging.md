# Branching and Merging
In the TEI repo, we have the following branches that are always around:

 * dev
 * master

Most of the Council's work happens in dev, and we only merge into master when we're tagging a release or release candidate. The idea here is that master is always in a releasable state. Council members and Contributors may create their own branches at any time, particularly to work on things that aren't quick fixes. If you are working on such an issue, and want to need to push your branch to GitHub either so others can see it or so you'll have it available, the standard procedure is to give it a name like hcayless_appcrit (to use a real example), that is, your GitHub username, followed by an underscore and a nickname for your project or issue. This lets other Council members or Contributors know who is responsible for the branch. You are not required to do this for private branches that you keep in your local repo, it's only for ones you want to share. If you called your branch "mybranch" and then later you decide you want to push it to GitHub, you can rename it:

	git branch -m mybranch myname_mybranch
## Creating a branch
Creating a branch is as easy as typing:

	git checkout -b myname_mybranch
This will create a new branch in your local repo named "myname_mybranch" and will switch you over to that branch. You can check that you're in your new branch by doing `git status`. Anything you do now will have no effect on dev or master until you merge your branch back into dev. If you need to push your branch up to GitHub, do

	git push -u origin myname_mybranch
and then you should be able to see your branch listed on the GitHub TEI repo.
## Keeping your branch up-to-date
Inevitably, work will go on in dev while you are working on your branch. In order to keep your eventual merge of your branch back into dev as simple as possible, you'll want to keep it in sync with the changes to dev. There are two ways of doing this. If your branch is private (you haven't pushed it to GitHub) then you may want to "rebase":

	git checkout dev
	git pull origin dev
	git checkout mybranch
	git rebase dev
What this does is take the latest commit from the dev branch and re-run all of the commits you've made to your branch on top of it. This avoids creating a special commit for the merge and may be a little simpler than merging. The second method, and the one you'll use most, is to merge:

	git checkout dev
	git pull origin dev
	git checkout mybranch
	git merge dev
Occasionally when this happens, you'll have merge conflicts. When that happens, Git will tell you, and give you a list of the files with conflicts it doesn't know how to resolve. These will have adjacent lines with conflict markers:

	<<<<<<< HEAD
	my text
	=======
	their text
	>>>>>>> dev
To fix this, you'll need to delete the conflict markers, pick "my text" or "their text" (or combine them somehow), and then save your file, `git add` and `git commit` it. The better you are about keeping your branch in sync with dev, the less likely it is you'll have to deal with this sort of thing.
## Merging back into dev
When the time comes to merge your changes back into dev, you should first make sure your branch is up to date with dev, so follow the procedure above. Then you'll need to do the same procedure in the dev branch:

	git checkout dev
	git merge --squash mybranch
	git commit
The --squash option applies all of the changes to files from your branch, but doesn't commit them. This means that when you commit, you won't be linking to the history that's in your branch. It will be "squashed" in dev into a single commit. There may be times when you don't want to do this, and that's fine, but doing it means that dev's commit history will be cleaner, and if you had lots of commits in your branch where you were just trying things or fixing typos, those minor commits won't surface. Note that the commit above doesn't have an "-m" flag and a message. If you do this, an editor will open to let you edit your commit message. For important commits like this, we recommend you write a message in the form:

	A short summary
	
	A longer and fuller description of what you did, what the significance of it was, etc., etc..
Doing this will give people reading the commit log a much better idea of what you did without producing a messy-looking message.