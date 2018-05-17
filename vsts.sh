curl -o run-elasmobranch-in-vsts.sh https://raw.githubusercontent.com/tomjaguarpaw/elasmobranch/binary/run-elasmobranch-in-vsts.sh
echo Checking integrity of downloaded file ...
echo "a3314989b411ff914355bbcf7816834fb59eed11ba0758a0c60c61b7c6e756bf  run-elasmobranch-in-vsts.sh" || exit 1
sh ./run-elasmobranch-in-vsts.sh
