## install jdk
```bash
# macos
brew install jenv
brew install openjdk@17
jenv add /opt/homebrew/Cellar/openjdk@17/17.0.13/libexec/openjdk.jdk/Contents/Home
jenv global 17
```

```bash
# ubuntu
echo 'export PATH="path-to-jdk-bin:$PATH"' >> ~/.bashrc
```

## install oss-cad-suite (include verilator)
```bash
# macos
curl -L https://github.com/YosysHQ/oss-cad-suite-build/releases/download/2023-12-01/oss-cad-suite-darwin-arm64-20231201.tgz -o ~/oss-cad-suite-darwin-arm64-20231201.tgz
cd ~
tar -xzvf oss-cad-suite-darwin-arm64-20231201.tgz
echo 'export PATH="$HOME/oss-cad-suite/bin:$PATH"' >> ~/.zshrc
source ~/.zshrc
```

```bash
# ubuntu
curl -L https://github.com/YosysHQ/oss-cad-suite-build/releases/download/2023-12-01/oss-cad-suite-linux-x64-20231201.tgz -o ~/oss-cad-suite-linux-x64-20231201.tgz
cd ~
tar -xzvf oss-cad-suite-linux-x64-20231201.tgz
echo 'export PATH="$HOME/oss-cad-suite/bin:$PATH"' >> ~/.bashrc
source ~/.bashrc
```