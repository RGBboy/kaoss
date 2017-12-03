# Kaoss

Music toy using the web audio API

## Known issues

There is currently a bug in webkit for mobile where changing from internal speaker to headphones causes strange audio artefacts. This seems to be related to the fact that the sample rate changes from 48,000 (internal) to 41,100 (headphones). Headphones seem to give the most consistent experience with desktop.

https://bugs.webkit.org/show_bug.cgi?id=154538
