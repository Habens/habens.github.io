name=""

new-draft:
	bundle exec octopress new draft $(name)

publish-blog:
	bundle exec octopress publish $(name) --dir blog

publish-article:
	bundle exec octopress publish $(name) --dir article

clean:
	bundle exec jekyll clean

run:
	bundle exec jekyll serve --config "_config-local.yml"
