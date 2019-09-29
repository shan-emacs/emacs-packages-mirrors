Evaluate expressions with proxy

,---
| (with-proxy
|   ...)
|
| ;; equals to:
| (with-proxy
|   :http-server "127.0.0.1:1081"
|   :no-proxy '("localhost" "127.0.0.1" "192.168.*" "10.*")
|   ...)
|
| ;; equals to:
| (with-proxy-url
|   :http-server "127.0.0.1:1081"
|   :no-proxy '("localhost" "127.0.0.1" "192.168.*" "10.*")
|   (with-proxy-shell
|     :http-server "127.0.0.1:1081"
|     ...))
`---

See README for more information.
