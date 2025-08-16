# 🚢 Shipguard - Blockchain Shipping Insurance & SLA Oracle

> Decentralized shipping delay insurance powered by blockchain oracles 📦⚡

## 🌟 Overview

Shipguard is a revolutionary smart contract platform that provides **automated shipping insurance** and **Service Level Agreement (SLA) enforcement** for logistics companies and shippers. Built on Stacks blockchain using Clarity smart contracts.

## ✨ Key Features

- 🛡️ **Automated Insurance Claims** - Get compensated automatically for delayed shipments
- 📊 **Oracle Integration** - Real-time shipping status updates from trusted oracles  
- 💰 **Dynamic Premiums** - Fair pricing based on insurance amount and risk
- 🔒 **Trustless Execution** - No intermediaries, pure blockchain automation
- 📈 **Transparent SLAs** - Clear delivery expectations with automatic enforcement

## 🚀 How It Works

1. **Create Shipment** 📋 - Shipper creates a shipment with expected delivery time and insurance amount
2. **Pay Premium** 💳 - Automatic premium calculation and payment to insurance pool
3. **Track Status** 📍 - Oracle updates shipment status throughout journey
4. **Claim Insurance** 🎯 - Automatic payouts for delayed deliveries based on SLA breach

## 💻 Usage

### For Shippers

```clarity
;; Create a new insured shipment
(contract-call? .shipguard create-shipment 'SP2RECIPIENT123 u1000 u50000)

;; Update shipment status (shipper or oracle)
(contract-call? .shipguard update-shipment-status u1 "in-transit")

;; Claim insurance for delayed shipment
(contract-call? .shipguard claim-insurance u1)
```

### For Recipients

```clarity
;; Confirm delivery
(contract-call? .shipguard confirm-delivery u1)
```

### For Oracles

```clarity
;; Update shipment status
(contract-call? .shipguard update-shipment-status u1 "delivered")
```

## 🔧 Setup & Deployment

### Prerequisites
- Clarinet CLI installed
- Stacks wallet configured

### Installation

```bash
git clone <repository-url>
cd shipguard
clarinet check
```

### Testing

```bash
clarinet test
```

### Deployment

```bash
clarinet deploy --testnet
```

## 📊 Contract Functions

### Public Functions
- `create-shipment` - Create new insured shipment
- `update-shipment-status` - Update shipment status
- `confirm-delivery` - Confirm successful delivery
- `claim-insurance` - Claim insurance for delays
- `deposit-funds` - Add funds to user balance
- `withdraw-funds` - Withdraw user funds

### Read-Only Functions
- `get-shipment` - Get shipment details
- `get-balance` - Check user balance
- `calculate-premium` - Calculate insurance premium
- `is-shipment-delayed` - Check if shipment is delayed

## 💡 Premium Calculation

Premium = (Insurance Amount × Base Rate) / 10,000

Default base rate: 1% (100/10000)

## 🎯 Payout Logic

- **Full Payout**: Delays > 100 blocks
- **Partial Payout**: Proportional to delay duration
- **No Payout**: On-time or early delivery

## 🛠️ Configuration

Contract owner can:
- Set oracle addresses
- Update premium rates
- Manage insurance pool

## 🔐 Security Features

- Role-based access control
- Overflow protection
- Balance validation
- Claim processing limits

## 📈 Future Enhancements

- 🌐 Multi-oracle consensus
- 📱 Mobile app integration  
- 🤖 AI-powered risk assessment
- 🌍 Cross-chain compatibility

## 🤝 Contributing

We welcome contributions! Please feel free to submit pull requests or open issues.

## 📄 License

MIT License - see LICENSE file for details


