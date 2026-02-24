#!/usr/bin/env bash
# Deploy NatureJust-EU to Shiny Server on laguna.ku.lt
# Usage: bash deploy.sh
#
# Prerequisites:
#   - Passwordless SSH access to laguna.ku.lt as razinka
#   - R packages installed on server
#   - /srv/shiny-server/naturejust writable by razinka

set -euo pipefail

# ── Configuration ─────────────────────────────────────────────
REMOTE_USER="razinka"
REMOTE_HOST="laguna.ku.lt"
REMOTE_DIR="/srv/shiny-server/naturejust"
REMOTE="${REMOTE_USER}@${REMOTE_HOST}"

# Project root = directory containing this script
PROJECT_DIR="$(cd "$(dirname "$0")" && pwd)"

# ── Colours ───────────────────────────────────────────────────
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m'

info()  { echo -e "${GREEN}[deploy]${NC} $*"; }
warn()  { echo -e "${YELLOW}[deploy]${NC} $*"; }
error() { echo -e "${RED}[deploy]${NC} $*" >&2; }

# ── Preflight checks ─────────────────────────────────────────
info "Checking SSH connectivity to ${REMOTE_HOST}..."
if ! ssh -o ConnectTimeout=5 "${REMOTE}" "echo ok" &>/dev/null; then
    error "Cannot reach ${REMOTE_HOST} via SSH. Check your connection."
    exit 1
fi
info "SSH connection OK."

# ── Ensure remote directory exists ────────────────────────────
info "Ensuring ${REMOTE_DIR} exists on server..."
ssh "${REMOTE}" "mkdir -p ${REMOTE_DIR}"

# ── Deploy via tar over SSH ───────────────────────────────────
# Clean remote directory, then stream a tar archive of only the
# files needed at runtime. This is portable (no rsync needed).
info "Deploying to ${REMOTE_HOST}:${REMOTE_DIR}..."

ssh "${REMOTE}" "rm -rf ${REMOTE_DIR}/R ${REMOTE_DIR}/inst ${REMOTE_DIR}/app.R"

cd "${PROJECT_DIR}"
tar cf - \
    app.R \
    R/ \
    inst/ \
    2>/dev/null | ssh "${REMOTE}" "tar xf - -C ${REMOTE_DIR}/"

info "File transfer complete."

# ── Set permissions ───────────────────────────────────────────
info "Setting file permissions..."
ssh "${REMOTE}" "chmod -R u+rw,g+r,o+r ${REMOTE_DIR}"

# ── Restart Shiny Server ──────────────────────────────────────
info "Restarting Shiny Server..."
if ssh "${REMOTE}" "sudo systemctl restart shiny-server 2>/dev/null"; then
    info "Shiny Server restarted."
else
    warn "Could not restart shiny-server (no sudo?). Restart manually or wait for auto-reload."
fi

# ── Verify ────────────────────────────────────────────────────
info "Verifying deployment..."
REMOTE_APP_R=$(ssh "${REMOTE}" "test -f ${REMOTE_DIR}/app.R && echo 'yes' || echo 'no'")
REMOTE_FILES=$(ssh "${REMOTE}" "ls ${REMOTE_DIR}/R/ | wc -l")

if [ "$REMOTE_APP_R" = "yes" ]; then
    info "app.R present: OK"
else
    error "app.R missing on server!"
    exit 1
fi

info "R/ files on server: ${REMOTE_FILES}"
info ""
info "Deployment complete!"
info "App should be available at: https://laguna.ku.lt/naturejust/"
