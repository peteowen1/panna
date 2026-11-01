# Read an asset's true stored size, bypassing the release listing

The release-listing endpoint can serve the PREVIOUS asset's row for a
few seconds after an upload (torpdata#74). When that happens there is no
way to tell a lagging read from a lost write out of the listing alone,
because every field in it — size, `updated_at`, even the asset `id` —
belongs to the stale row. Asking by `id` therefore just re-confirms the
stale answer.

## Usage

``` r
.vb_asset_true_size(repo, tag, file_name)
```

## Arguments

- repo:

  "owner/name".

- tag:

  Release tag.

- file_name:

  Asset file name, including extension.

## Value

Size in bytes, or `NA_real_` if it could not be determined.

## Details

The release DOWNLOAD path resolves by **name**, so it cannot hand back
the previous asset. A one-byte ranged GET against it costs a single byte
on the wire and reports the object's real length in `Content-Range`.

Best-effort by contract: **every** failure path returns `NA_real_`,
including a private repo whose download redirect needs credentials we do
not have. This must never become the reason an upload that succeeded is
reported as failed — callers fall back to whatever they did before.
