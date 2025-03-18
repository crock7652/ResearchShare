# ResearchShare

A decentralized academic research sharing platform built on Stacks blockchain using Clarity smart contracts.

## Overview

ResearchShare enables researchers to:
- Submit and share academic papers
- Verify research papers
- Track citations between papers
- Participate in peer review processes

## Features

- **Paper Submission**: Researchers can submit papers with titles and abstracts
- **Paper Verification**: Submitted papers can be verified (could be restricted to authorized verifiers)
- **Citation Tracking**: Track and verify citations between papers
- **Peer Review System**: Submit and view paper reviews with scores and comments

## Smart Contract Functions

### Paper Management
- `submit-paper`: Submit a new research paper
- `get-paper`: Retrieve paper details
- `verify-paper`: Verify a submitted paper
- `get-paper-count`: Get total number of papers in the system

### Review System
- `submit-review`: Submit a review for a paper
- `get-review`: Retrieve a specific review

### Citation System
- `add-citation`: Add a citation between papers
- `has-citation`: Check if a paper cites another paper

## Development

### Prerequisites
- [Clarinet](https://github.com/hirosystems/clarinet)
- Node.js and npm

### Setup
1. Clone the repository
2. Install dependencies:
```bash
npm install

npm test

npm run test:report

research-share/
├── contracts/           # Clarity smart contracts
├── tests/              # Test files
├── settings/           # Network configuration
└── deployments/        # Deployment configurations

