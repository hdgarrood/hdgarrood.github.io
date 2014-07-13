set -ex

ROOT_DIR="$(cd "$(dirname ${BASH_SOURCE[0]})" && cd .. && pwd)"
PANDOC="/home/harry/build/cabal-sandboxes/pandoc/.cabal-sandbox/bin/pandoc"

pushd "$ROOT_DIR"

# styles
sass assets/css/styles.scss >assets/css/styles.css

# literate haskell
echo -e "---\nlayout: post\n---\n" > index.html

"$PANDOC" \
    --from=markdown+lhs \
    --to=html \
    blog/permutations/permutations.lhs \
    >> blog/permutations/index.html

jekyll build

popd
