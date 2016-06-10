# overblow

Add-on library for use with Overtone in Clojure.

# Getting Started

## On Windows 10 (64-bit)

### Prerequisites

SuperCollider 3.6.6, leiningen 2.6.1, Java 1.8.0

### Starting up

1) Start SuperCollider 3.6.6.
2) Open a command console and type `lein repl`.

```clojure
(use 'overtone.core)
(boot-external-server)
(demo (sin-osc 440))
```
