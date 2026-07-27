rm -rf dist build
bunx elm-watch make --optimize
mkdir dist
cp -r index.html elm-audio.js style.css build media dist
cd dist
rm -f ../dist.zip
zip -r ../dist.zip .
