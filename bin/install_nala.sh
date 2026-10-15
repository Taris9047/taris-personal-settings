#!/bin/bash

##############################################################################
#                                                                            #
# Nala source installation script.                                           #
#                                                                            #
# --> Nala was a python based apt-get or apt wrapper. but now they are       #
# based on rust. So, we need some Rust tools to install this properly.       #
#                                                                            #
##############################################################################\

# Exit on error
set -e

# Define a temporary workspace to keep things organized
WORK_DIR=$(mktemp -d)
echo "📂 Creating temporary workspace: $WORK_DIR"

# Ensure cleanup happens even if the script fails (on error or exit)
cleanup() {
    echo ""
    echo "🧹 Cleaning up..."
    rm -rf "$WORK_DIR"
    echo "✅ Workspace cleared."
}
trap cleanup EXIT

# 0. Installing prerequisite packages. Assuming Ubuntu based a the moment.
sudo apt-get install -y g++ libapt-pkg-dev pkgconf codespell

# 1. Clone the repository into the temp workspace
echo "📥 Cloning volitank/nala..."
git clone https://github.com/volitank/nala.git "$WORK_DIR/nala"

# 2. Enter the directory
cd "$WORK_DIR/nala"

# 3. Compile the project
echo "🛠️ Compiling in release mode (this may take a few minutes)..."
cargo build --release

# 4. Installation Step
echo ""
echo "✅ Build Complete!"
echo "-----------------------------------------------------------"
echo "Ready to install."
read -p "Do you want to install nala to /usr/local/bin/ now? (y/n): " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
    echo "🚀 Installing..."
    sudo cp target/release/nala /usr/local/bin/
    echo "🎉 Successfully installed to /usr/local/bin/nala"
else
    echo "⚠️ Installation skipped. The binary is sitting in $WORK_DIR/nala/target/release/nala"
    echo "The temporary folder will be deleted when you exit this script."
fi
echo "-----------------------------------------------------------"
