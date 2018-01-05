.PHONY: publish clean

DOCKER_LINUX_IMAGE="fpco/stack-build:lts-10.2"
API_HOST=https://api.github.com
UPLOAD_HOST=https://uploads.github.com
DASH_VERSION=$(shell echo $(VERSION) | sed -e s/\\./-/g)

ifdef GITHUB_TOKEN
	AUTH=-H 'Authorization: token $(GITHUB_TOKEN)'
endif


# Utility target for checking required parameters
guard-%:
	@if [ "$($*)" = '' ]; then \
		echo "Missing required $* variable."; \
		exit 1; \
	fi;

dist-linux/docker-build-cacher:
	mkdir -p dist-linux
	stack --docker --docker-auto-pull --docker-image $(DOCKER_LINUX_IMAGE) install --local-bin-path dist-linux
	upx --best dist-linux/docker-build-cacher

dist-macos/docker-build-cacher:
	mkdir -p dist-macos
	stack install --local-bin-path dist-macos
	upx --best dist-macos/docker-build-cacher

release.json: dist-linux/docker-build-cacher dist-macos/docker-build-cacher
	@echo "Creating draft release for $(VERSION)"
	@curl $(AUTH) -XPOST $(API_HOST)/repos/seatgeek/docker-build-cacher/releases -d '{ \
		"tag_name": "$(VERSION)", \
		"name": "Docker build cacher $(VERSION)", \
		"draft": false, \
		"prerelease": false \
	}' > release.json
	@echo "Uploading binaries to github"

publish: guard-VERSION guard-GITHUB_TOKEN release.json
	$(eval RELEASE_ID := $(shell cat release.json | jq .id))
	@sleep 1
	@echo "Uploading the Linux docker-build-cacher"
	@curl $(AUTH) -XPOST \
		$(UPLOAD_HOST)/repos/seatgeek/docker-build-cacher/releases/$(RELEASE_ID)/assets?name=docker-build-cacher-linux \
		-H "Accept: application/vnd.github.manifold-preview" \
		-H 'Content-Type: application/octet-stream' \
		--data-binary '@dist-linux/docker-build-cacher' > /dev/null
	@echo "Uploading the MacOS binary"
	@curl $(AUTH) -XPOST \
		$(UPLOAD_HOST)/repos/seatgeek/docker-build-cacher/releases/$(RELEASE_ID)/assets?name=docker-build-cacher-macos \
		-H "Accept: application/vnd.github.manifold-preview" \
		-H 'Content-Type: application/octet-stream' \
		--data-binary '@dist-macos/docker-build-cacher' > /dev/null
	@echo Release done, you can go to:
	@cat release.json | jq .html_url


clean:
	rm -rf dist-*
	rm -f release.json
