#!/bin/sh
# Fetch a headless Pharo once into pharo/.image, load the packages, run the tests.
set -e
dir=$(cd "$(dirname "$0")" && pwd)
img=$dir/.image
if [ ! -f "$img/Pharo.image" ]; then
  mkdir -p "$img" && (cd "$img" && curl -sL https://get.pharo.org/64/130+vm | bash)
fi
cat > "$img/tests.st" <<TESTS
| result |
Metacello new baseline: 'Yuuki'; repository: 'tonel://$dir/src'; load.
result := TestResult new.
#(YuukiCodexTest YuukiToolTest YuukiAgentTest) do: [ :name | (Smalltalk globals at: name) suite run: result ].
Transcript show: result printString; cr.
result failures do: [ :f | Transcript show: 'FAIL ' , f printString; cr ].
result errors do: [ :f | Transcript show: 'ERROR ' , f printString; cr ].
(result hasFailures or: [ result hasErrors ]) ifTrue: [ Smalltalk exit: 1 ].
Smalltalk exit: 0.
TESTS
cd "$img" && exec ./pharo Pharo.image st tests.st 2>&1 | grep -vE '^$|MetacelloNotification'
