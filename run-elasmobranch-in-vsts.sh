artifact_upload () {
    ARTIFACTNAME="$1"
    LOCALFILEPATH="$2"
    CONTAINEROLDER="$ARTIFACTNAME"
    LOCALPATH="$LOCALFILEPATH"

    echo "##vso[artifact.upload containerfolder=$CONTAINERFOLDER;artifactname=$ARTIFACTNAME;localpath=$LOCALFILEPATH;]$LOCALFILEPATH"
}

die() {
    MESSAGE=$1
    echo $MESSAGE
    exit 1
}

verified_download() {
    URL="$1"
    OUTPUT="$2"
    CHECKSUM="$3"

    curl -o "$OUTPUT" "$URL"
    echo "$CHECKSUM  $OUTPUT" | sha256sum --strict --check --status || die "Hash of $OUTPUT was wrong!"
}

ARTIFACT_DIRECTORY=Elasmobranch

mkdir -p $ARTIFACT_DIRECTORY
verified_download 'https://github.com/tomjaguarpaw/elasmobranch/raw/145e7e4bcf6044a140e2c815fe4bd216d5247420/matrix2' matrix 45f1e4175b87020f0315aad9f5e2354b0d4dd3acb93bb057f23b8011f9320fcf
chmod +x matrix
./matrix . $ARTIFACT_DIRECTORY/elasmobranch.html

artifact_upload $ARTIFACT_DIRECTORY "$PWD/$ARTIFACT_DIRECTORY"
