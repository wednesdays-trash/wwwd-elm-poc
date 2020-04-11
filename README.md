# A fun proof of concept written in Elm

## Usage

```shell
$ elm make src/Main.elm
```

Open index.html created at the directory.

## Breif Description

This version fetches a list of high ranked players for a given hero, then for each player looks for a match in which they're facing the given opposing hero. This way is bad because [OpenDota's API parameters](https://docs.opendota.com/#tag/players%2Fpaths%2F~1players~1%7Baccount_id%7D~1matches%2Fget) don't expose lane filtering. To filter by lane, we have to fetch the whole match details, then check whether the lanes satisfy the requirements. This can get costly very quickly; OpenDota's API is limited to 60 requests per minute, and this method chews it up. Also, it's slow!
