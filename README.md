# sc-build

Need both manual weebohook and app.

Installation id needs to be configured manually.

For macos docker API:

```bash
socat TCP-LISTEN:1234,range=127.0.0.1/32,reuseaddr,fork UNIX-CLIENT:/var/run/docker.sock
```

or this (didn't work for me):

```bash
docker run -d -v /var/run/docker.sock:/var/run/docker.sock -p 127.0.0.1:2357:1234 bobrik/socat TCP-LISTEN:1234,fork UNIX-CONNECT:/var/run/docker.sock
```
