# xrouter
Library for interacting with XMPP components. So you can connect it using any XMPP library which implements [XEP 0114](https://xmpp.org/extensions/xep-0086.html).

# Build
### Download source

##### Git
```sh
~ $ git clone https://github.com/soroush-app/xrouter.git
```

### Use as dependency

##### Rebar
Put this in `deps` in `rebar.config`:  
```erlang
{xrouter, ".*", {git, "https://github.com/soroush-app/xrouter.git", {tag, "17.9.3"}}}
```
