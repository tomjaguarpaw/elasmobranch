set -e

DIR=tmp-git-`date -Is`

echo $DIR
mkdir $DIR
cd $DIR

git init

touch dummy
git add dummy
git commit -m "Dummy commit to get a master branch"

git checkout -b 1 master
echo 1 > a
git add a
git commit -m "Add 1 to a"

git checkout -b 2 master
echo 2 > a
git add a
git commit -m "Add 2 to a"


