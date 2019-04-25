So I'm going to be working with Clojure for a bit. Like, at work. And as a result I've had to deal with some minor infrastructure issues I thought I'd share.

## S3 Wagon

Setting up a private repository for Clojure libraries is really straightforward. Usually, when I'm working on something, I'll toss it up onto [Clojars](TODO), but _this_ time, I don't want these repos to be fully public. They're pieces of internal infrastructure that deal with how our deploys are going to work, and possibly reveal some internals that we'd rather keep proprietary for the moment. The alternative I settled on was using [`s3-wagon-private`](https://github.com/s3-wagon-private/s3-wagon-private) to host a bunch of library `jar`s on a private bucket. This effectively lets you run your own `maven` repository.

There are only a couple fiddly bits. The [step-by-step guide](https://github.com/s3-wagon-private/s3-wagon-private/wiki/Creating-a-Private-Clojure-component-Step-By-Step-Guide) does a pretty good job of getting you through it.

There are two bits to the workflow that I'd want to automate, or at least semi-automate.

Firstly, every time I edit one of these libraries, I'll want to be able to push it up to our local repository with a minimum of fuss. Secondly, any new project I start for work will need to be able to pull things from those repos.

## Pushing

The first bit is reasonably simple; I'll want a shell script that just does the thing. That's fairly straightforward; it looks like

```
#! /bin/sh

BUCKET="org.your-org-name.clj"
PATH="org/your-org-name/clj"

VERSION=`head -n 1 project.clj | grep -E -o '[0-9]+\.[0-9]+\.[0-9]+'`
PROJECT=`head -n 1 project.clj | grep -oP ' \K([^ "]+)'`

echo "Building uberjar..."
lein uberjar
echo "Deploying locally..."
eval "mvn deploy:deploy-file -Dfile=target/$PROJECT-$VERSION-SNAPSHOT.jar -DartifactId=$PROJECT -Dversion=$VERSION -DgroupId=$BUCKET -Dpackaging=jar -Durl=file:maven_repository -Dmaven.repo.local=maven_repository -DcreateChecksum"
echo "Copying to S3..."
eval "aws s3 cp maven_repository/$PATH/$PROJECT/$VERSION s3://$BUCKET/releases/$PATH/$PROJECT/$VERSION --recursive"
rm -r maven_repository
echo "Done"
```

This can actually just be a global script. So you might add it as a function to your shell `rc` file, or you might keep it somewhere on your `$PATH` and just execute it once. Assuming you have your AWS credentials set, and `maven` and `lein` installed, running this in a project directorythis will

1. Build the project
2. Deploy to a temporary local `maven` repository
3. Copy that subtree up to your `S3` bucket
4. Clean up the temporary local repository

There. Done.

## Pulling

The other part is slightly more annoying, because it involves adding stuff to every `lein` project you create that uses your private repo. You need to remember to add

```
  :plugins [[s3-wagon-private "1.1.2"]]
  :repositories {"bear-private"
                 {:url "s3p://org.your-org-name.clj/releases/"
                  :username :env/aws_access_key_id
                  :passphrase :env/aws_secret_access_key}}
```

to your new repos. Forgetting to add the `repositories` value is pretty easy to diagnose, but forgetting to add that `plugins` line gives you comparably cryptic messages about required projects not being found in the main `maven`/`clojars` registries. Oh, in addition to the above, you probably also want to add the entries for any core libraries your projects use up in the `dependencies` section, and show `require` examples over in `core.clj`.

All of this tells me that what I really want is a new project template.

- creating with `create-template`
- editing existing files
- editing to add new files
