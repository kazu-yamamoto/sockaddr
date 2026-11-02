# ChangeLog for sockaddr

## 0.1.0

The output format of IPv6 addresses is changed.

- IPv6 addresses are compressed according to RFC 5952.
- Non-zero scope IDs of IPv6 addresses are appended, e.g. `fe80::1%4`.
- Using `Builder` instead of `printf`.
- Dropping the dependency on `byteorder`.
- Adding a test suite.

## 0.0.1

- Initial release.
