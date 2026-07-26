rm -rf dist build
bunx elm-watch make
mkdir dist
cp -r index.html style.css build media dist
cd dist
rm -f ../dist.zip
zip -r ../dist.zip .
