set -ex

ROOT_DIR="$(cd "$(dirname ${BASH_SOURCE[0]})" && cd .. && pwd)"
PANDOC="/home/harry/build/cabal-sandboxes/pandoc/.cabal-sandbox/bin/pandoc"

pushd "$ROOT_DIR"

# styles
sass assets/css/styles.scss >assets/css/styles.css

# literate haskell
cat > _drafts/permutations.html <<END
---
layout: post
title: Permutations
---
END

"$PANDOC" \
    --from=markdown+lhs \
    --to=html+lhs \
    blog/permutations/permutations.lhs \
    >> _drafts/permutations.html

popd