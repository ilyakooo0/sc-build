# sc-build

Need both manual weebohook and app.

Installation id needs to be configured manually.

## Usage

You want to have some predefined docker container with maximum dependencies cached.


Exmaple:

```dockerfile
FROM fpco/stack-build:lts-14.27

RUN stack update
RUN stack build tasty tasty-hspec aeson containers bytestring

WORKDIR /
```

You need to build that container and git it a name:

```bash
docker build -t fast-lts-14.27 - < FastDockerFile
```

After that you can use it in your `tasks.yaml`:

```yaml
test-task:
  prebuild: |
    cp -f ../../test-task/package.yaml .
    cp -f ../../test-task/stack.yaml .
    cp -f -R ../../test-task/test .
  dockerfile: |
    FROM fast-lts-14.27
    COPY . src
    WORKDIR /src
    ENTRYPOINT stack test --fast
  timeoutMinutes: 10
```

The docker container is expected to write json to stdout, conating test results:

```json
{
  "test1": true,
  "test2": false
}
```

For macos docker API:

```bash
socat TCP-LISTEN:1234,range=127.0.0.1/32,reuseaddr,fork UNIX-CLIENT:/var/run/docker.sock
```

or this (didn't work for me):

```bash
docker run -d -v /var/run/docker.sock:/var/run/docker.sock -p 127.0.0.1:2357:1234 bobrik/socat TCP-LISTEN:1234,fork UNIX-CONNECT:/var/run/docker.sock
```
