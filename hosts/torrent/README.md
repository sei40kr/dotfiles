# Torrent Host Configuration

## Work VPN (OpenVPN) Client Setup

This host is configured to connect to the work VPN as a client using OpenVPN.

The configuration is managed as follows:

- **`agenix` managed files**: Public information (CA certificate, remote server list) is encrypted and committed to this repository. You do not need to do anything about these files.
- **Manually managed files**: Sensitive information (TLS crypt key, authentication credentials) must be created by you on the target machine.

### Provide Manually Managed Secrets

You need to manually create the following two files on the `torrent` machine after applying the NixOS configuration. These files contain sensitive information and should **not** be committed to Git.

#### 1. TLS Crypt Key

Create a file containing the TLS crypt key provided by your VPN administrator.

```bash
sudo touch /var/lib/openvpn/work-tc.key
sudo chmod 600 /var/lib/openvpn/work-tc.key
# Now, paste the key content into the file. For example:
# sudo vim /var/lib/openvpn/work-tc.key
```

#### 2. Authentication Credentials

Create a file for your VPN username and password.

```bash
sudo touch /var/lib/openvpn/work-auth.txt
sudo chmod 600 /var/lib/openvpn/work-auth.txt
# Edit the file to contain two lines: username on the first, password on the second.
# sudo vim /var/lib/openvpn/work-auth.txt
```

### Starting the VPN

Once all secret files are in place, you can manually start the OpenVPN client service:

```bash
sudo systemctl start openvpn-work.service
```
