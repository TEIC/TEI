# Notes on using Git to edit the Guidelines

## Where do I start?
Well, first, you'll need Git. You may already have it, but if not, see 
<https://git-scm.com/book/en/v2/Getting-Started-Installing-Git> for some helpful guidance. There are some GUI interfaces
to GitHub available at <https://windows.github.com> and <https://mac.github.com> if you prefer a graphical alternative to 
the command line. <https://git-scm.com/doc>, by the way is a comprehensive, and pretty readable resource. It's good to have 
it handy. You should set your name and email address, so Git will be able to record it in your commits.

    git config --global user.name "John Doe"
    git config --global user.email johndoe@example.com

will do the trick. See also <https://git-scm.com/book/en/v2/Getting-Started-First-Time-Git-Setup>.

Next, you should clone the repo. If you want to be able to make changes and push them back to this repo (and you are 
encouraged to do so, they won't be kept, so you can't break anything), you should log in to GitHub, creating an account 
if you don't have one yet. If you haven't been added to the TEI Technical Council team yet, and I believe a couple of us 
haven't, then email the Council list and ask to be added with your GitHub username. Once you are logged in and have been 
given access, you should be able to clone the repo using the URL on the right side of the repo page. You can clone anyway, 
even without signing in, but you won't be able to push your changes back.

## Ok, I cloned it, now what?
Git is a bit different from Subversion, which you're used to using. For one thing, your cloned repo is a complete copy of 
the Guidelines repository. GitHub has nothing that you don't. The commands are similar to what you do with `svn`, but when 
you commit, you're committing to your local copy of the repo, not to the server copy.

### Remotes
The first bit of difference is that when you want to sync your local copy with the server (if someone has pushed changes
to GitHub, for example), you'll want to pull them down, using a command like `git pull origin master`. "Git pull" is the 
command, "origin" is a nickname for the URL you cloned the repo from (GitHub in this case). You can have other remotes, 
and they don't have to be called "origin", it's just a convention.

### Branches
The "master" part of your pull command is a branch name. Again, this is a convention, but you can think of it like "trunk"
in subversion. Branches in Git are very different from branches in svn. In the latter, they are copies of your working
directories. In Git they are alternate paths in the commit history. You're not bloating the repo by creating branches in
other words, you can use them as much as you like. You can create a new branch by issuing a `git checkout -b newbranchname` 
command. This creates a new branch, starting at the current commit (or HEAD in Git terminology) and switches you into it.
Changes you make here won't be visible in master until and unless you merge them back in. For more on branching, see 
<https://git-scm.com/book/en/v2/Git-Branching-Basic-Branching-and-Merging>.

Ok, so we're in master, and you just ran a pull command to make sure you're up-to-date with the copy on GitHub. Now you want 
to make a change and send it back to the server. Unlike svn, where this is just one command (`svn commit`), in Git, changes
have to be committed locally before they are pushed back. There are a couple of ways to do this. The simple one, where you
just want to commit all of your changes, would use a command like `git commit -am "My helpful commit message"`. This says
"Commit all of the changes in the repo that you see with the following message." If your change involved adding new files,
or if you didn't want to put all of your changes into a single commit, you'd use `git add filename.xml` first, to "stage"
your file, and then you'd run `git commit -m "My message"` (without the `-a`, you'll note).

### Where am I?
If, at any point, you want to see what the current state of your working copy is, you can run `git status`, which will tell 
you several interesting things like what branch you're on, what changes you have staged (i.e. that will get committed next 
time you run `git commit`), what changes you've made that aren't staged yet, and a list of untracked files. In the Guidelines, 
you might have several because you built the HTML version, for example. These are just lying around and Git has nothing to 
do with them unless you add them.

Now that you've committed a change to your local repo, if you run `git log`, you'll see your commit at the top along with other
helpful information like the commit "hash", which will be a value like `9ddca0d4394666bb82be94dc2cc7921815d12618`. Note that in
Git, the log is displayed using a pager program, so you don't get the whole log spewed at you in one go. You can advance a page
at a time using the space bar, quit by hitting 'q', and even search the log. The commit at the top of the log is your HEAD.
These commit ids look terrifying, but they're just unique identifiers for objects in git, which has trees (directories) and
blobs (files) as well as commits. They are derived from the contents of the object itself, so they're guaranteed to be unique
(collisions are theoretically possible, but I'm told there is a *much* higher probablility that we'll all be attacked and 
killed by wolves in unrelated incidents on a single night). So they look hideous because they're actually doing something very
useful. They're not sequential because history in Git can actually be manipulated in various ways.

If you then want to send your changes back to GitHub, you just run `git push origin master`. Remember, "origin" is just 
shorthand for the URL where you cloned from, and "master" is the branch you want to push. It's possible Git will tell you
no you can't do that, because someone else has pushed changes to GitHub before you did, so your repo is out of sync with the 
remote. In that case, you just run another `git pull origin master` and the remote changes will be merged. There's always the 
possibility of a merge conflict, and these are resolved the same way they are in svn, you look at the affected files, get rid 
of the bits you don't want along with the conflict markers, and go through the add-commit cycle above again. Now, if you 
couldn't before, you can run `git push origin master` and your changes will be synced to the remote, and you'll be able to see 
them here.


