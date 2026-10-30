# Security Policy

Rules for what information may be committed to this repository. Encrypt with agenix (`secrets.nix`, `.age` files).

| Information | Handling |
| --- | --- |
| My public information | May be committed in plain text |
| My confidential information | May be committed if encrypted |
| Work information (even if not confidential) | May be committed if encrypted |
| Confidential work information | Must never be committed |

## 1. My public information

May be committed in plain text.

## 2. My confidential information

May be committed if encrypted.

However, do not commit anything that would give access to confidential work information if leaked, such as credentials or private keys for company systems.

## 3. Work information

Encrypt it even when it is not confidential.

- Identifying a client would let others infer the client's internal affairs from my blog and social media posts.
- Company intranet domains are better kept unknown, even when access-controlled.

## 4. Confidential work information

Must never be committed, encrypted or not.
