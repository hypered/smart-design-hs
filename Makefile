all: \
  _site/index.html \
  _site/app-form.html \
  _site/blog-post.html


_site/%.html: src/Smart/Html/Application.hs src/Smart/Html/Website.hs
	mkdir -p $(dir $@)
	runghc -isrc/ bin/generate.hs --pretty $* > $@