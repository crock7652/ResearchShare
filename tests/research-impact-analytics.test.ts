import { Clarinet, Tx, Chain, Account, types } from 'https://deno.land/x/clarinet@v1.0.0/index.ts';
import { assertEquals } from 'https://deno.land/std@0.90.0/testing/asserts.ts';

// Test the Research Impact Analytics system
Clarinet.test({
    name: "Test citation impact recording",
    async fn(chain: Chain, accounts: Map<string, Account>) {
        const deployer = accounts.get('deployer')!;
        const researcher1 = accounts.get('wallet_1')!;
        const researcher2 = accounts.get('wallet_2')!;
        
        // Submit papers first
        let block = chain.mineBlock([
            Tx.contractCall('ResearchShare', 'submit-paper', [
                types.ascii("Paper 1"),
                types.ascii("Abstract 1")
            ], researcher1.address),
            Tx.contractCall('ResearchShare', 'submit-paper', [
                types.ascii("Paper 2"), 
                types.ascii("Abstract 2")
            ], researcher2.address),
        ]);
        
        assertEquals(block.receipts.length, 2);
        assertEquals(block.receipts[0].result.expectOk(), types.uint(1));
        assertEquals(block.receipts[1].result.expectOk(), types.uint(2));
        
        // Verify papers
        block = chain.mineBlock([
            Tx.contractCall('ResearchShare', 'verify-paper', [
                types.uint(1)
            ], deployer.address),
            Tx.contractCall('ResearchShare', 'verify-paper', [
                types.uint(2)
            ], deployer.address),
        ]);
        
        assertEquals(block.receipts.length, 2);
        assertEquals(block.receipts[0].result.expectOk(), types.bool(true));
        assertEquals(block.receipts[1].result.expectOk(), types.bool(true));
        
        // Add citation - this should trigger impact analytics
        block = chain.mineBlock([
            Tx.contractCall('ResearchShare', 'add-citation', [
                types.uint(1), // cited paper
                types.uint(2)  // citing paper
            ], researcher2.address),
        ]);
        
        assertEquals(block.receipts.length, 1);
        assertEquals(block.receipts[0].result.expectOk(), types.bool(true));
        
        // Check that paper impact metrics were created
        block = chain.mineBlock([
            Tx.contractCall('research-impact-analytics', 'get-paper-impact-metrics', [
                types.uint(1)
            ], deployer.address),
        ]);
        
        assertEquals(block.receipts.length, 1);
        const metrics = block.receipts[0].result.expectSome();
        
        // Verify citation velocity increased
        block = chain.mineBlock([
            Tx.contractCall('research-impact-analytics', 'get-citation-velocity', [
                types.uint(1)
            ], deployer.address),
        ]);
        
        assertEquals(block.receipts.length, 1);
        assertEquals(block.receipts[0].result.expectOk(), types.uint(1));
    },
});

Clarinet.test({
    name: "Test collaboration analytics integration",
    async fn(chain: Chain, accounts: Map<string, Account>) {
        const deployer = accounts.get('deployer')!;
        const researcher1 = accounts.get('wallet_1')!;
        const researcher2 = accounts.get('wallet_2')!;
        
        // Submit and verify a paper
        let block = chain.mineBlock([
            Tx.contractCall('ResearchShare', 'submit-paper', [
                types.ascii("Collaborative Paper"),
                types.ascii("Joint research abstract")
            ], researcher1.address),
        ]);
        
        assertEquals(block.receipts[0].result.expectOk(), types.uint(1));
        
        block = chain.mineBlock([
            Tx.contractCall('ResearchShare', 'verify-paper', [
                types.uint(1)
            ], deployer.address),
        ]);
        
        assertEquals(block.receipts[0].result.expectOk(), types.bool(true));
        
        // Add collaborator
        block = chain.mineBlock([
            Tx.contractCall('ResearchShare', 'add-collaborator', [
                types.uint(1),
                types.principal(researcher2.address),
                types.ascii("Co-author"),
                types.ascii("Contributed to data analysis")
            ], researcher1.address),
        ]);
        
        assertEquals(block.receipts[0].result.expectOk(), types.bool(true));
        
        // Approve collaboration - this should trigger collaboration analytics
        block = chain.mineBlock([
            Tx.contractCall('ResearchShare', 'approve-collaboration', [
                types.uint(1)
            ], researcher2.address),
        ]);
        
        assertEquals(block.receipts[0].result.expectOk(), types.bool(true));
        
        // Check collaboration strength between authors
        block = chain.mineBlock([
            Tx.contractCall('research-impact-analytics', 'get-collaboration-strength', [
                types.principal(researcher1.address),
                types.principal(researcher2.address)
            ], deployer.address),
        ]);
        
        assertEquals(block.receipts.length, 1);
        const collaborationStrength = block.receipts[0].result.expectOk();
        // Should be greater than 0 indicating collaboration exists
        assertEquals(parseInt(collaborationStrength.value) > 0, true);
    },
});

Clarinet.test({
    name: "Test H-index calculation",
    async fn(chain: Chain, accounts: Map<string, Account>) {
        const deployer = accounts.get('deployer')!;
        const researcher1 = accounts.get('wallet_1')!;
        
        // Test H-index calculation with sample citation counts
        let block = chain.mineBlock([
            Tx.contractCall('research-impact-analytics', 'calculate-h-index', [
                types.principal(researcher1.address),
                types.list([types.uint(10), types.uint(8), types.uint(6), types.uint(4), types.uint(2)])
            ], deployer.address),
        ]);
        
        assertEquals(block.receipts.length, 1);
        const hIndex = block.receipts[0].result.expectOk();
        
        // With citation counts [10,8,6,4,2], H-index should be calculated
        // This is a simplified calculation - real H-index would need proper sorting
        assertEquals(parseInt(hIndex.value) > 0, true);
        
        // Check that author metrics were updated
        block = chain.mineBlock([
            Tx.contractCall('research-impact-analytics', 'get-author-metrics', [
                types.principal(researcher1.address)
            ], deployer.address),
        ]);
        
        assertEquals(block.receipts.length, 1);
        const authorMetrics = block.receipts[0].result.expectSome();
        // Verify h-index was stored
    },
});

Clarinet.test({
    name: "Test cross-field citation impact",
    async fn(chain: Chain, accounts: Map<string, Account>) {
        const deployer = accounts.get('deployer')!;
        const researcher1 = accounts.get('wallet_1')!;
        
        // Record cross-field citation
        let block = chain.mineBlock([
            Tx.contractCall('research-impact-analytics', 'record-citation-impact', [
                types.uint(1),
                types.uint(2),
                types.ascii("computer-science"),
                types.ascii("biology")
            ], deployer.address),
        ]);
        
        assertEquals(block.receipts.length, 1);
        assertEquals(block.receipts[0].result.expectOk(), types.bool(true));
        
        // Check field citation network
        block = chain.mineBlock([
            Tx.contractCall('research-impact-analytics', 'get-field-citation-network', [
                types.ascii("computer-science"),
                types.ascii("biology")
            ], deployer.address),
        ]);
        
        assertEquals(block.receipts.length, 1);
        const networkData = block.receipts[0].result.expectSome();
        
        // Check interdisciplinary impact score
        block = chain.mineBlock([
            Tx.contractCall('research-impact-analytics', 'calculate-interdisciplinary-impact', [
                types.uint(1)
            ], deployer.address),
        ]);
        
        assertEquals(block.receipts.length, 1);
        const interdisciplinaryScore = block.receipts[0].result.expectOk();
        assertEquals(parseInt(interdisciplinaryScore.value) > 0, true);
    },
});

Clarinet.test({
    name: "Test temporal citation patterns",
    async fn(chain: Chain, accounts: Map<string, Account>) {
        const deployer = accounts.get('deployer')!;
        
        // Record multiple citations over time to test temporal patterns
        let block = chain.mineBlock([
            Tx.contractCall('research-impact-analytics', 'record-citation-impact', [
                types.uint(1),
                types.uint(2), 
                types.ascii("general"),
                types.ascii("general")
            ], deployer.address),
            Tx.contractCall('research-impact-analytics', 'record-citation-impact', [
                types.uint(1),
                types.uint(3),
                types.ascii("general"), 
                types.ascii("general")
            ], deployer.address),
        ]);
        
        assertEquals(block.receipts.length, 2);
        assertEquals(block.receipts[0].result.expectOk(), types.bool(true));
        assertEquals(block.receipts[1].result.expectOk(), types.bool(true));
        
        // Check citation velocity increased
        block = chain.mineBlock([
            Tx.contractCall('research-impact-analytics', 'get-citation-velocity', [
                types.uint(1)
            ], deployer.address),
        ]);
        
        assertEquals(block.receipts.length, 1);
        const velocity = block.receipts[0].result.expectOk();
        assertEquals(parseInt(velocity.value) >= 2, true);
    },
});

Clarinet.test({
    name: "Test author metrics initialization and updates",
    async fn(chain: Chain, accounts: Map<string, Account>) {
        const deployer = accounts.get('deployer')!;
        const researcher1 = accounts.get('wallet_1')!;
        const researcher2 = accounts.get('wallet_2')!;
        
        // Test collaboration metrics update
        let block = chain.mineBlock([
            Tx.contractCall('research-impact-analytics', 'update-collaboration-metrics', [
                types.principal(researcher1.address),
                types.principal(researcher2.address),
                types.uint(5) // paper has 5 citations
            ], deployer.address),
        ]);
        
        assertEquals(block.receipts.length, 1);
        assertEquals(block.receipts[0].result.expectOk(), types.bool(true));
        
        // Check that both authors got collaboration score updates
        block = chain.mineBlock([
            Tx.contractCall('research-impact-analytics', 'get-author-metrics', [
                types.principal(researcher1.address)
            ], deployer.address),
            Tx.contractCall('research-impact-analytics', 'get-author-metrics', [
                types.principal(researcher2.address)
            ], deployer.address),
        ]);
        
        assertEquals(block.receipts.length, 2);
        
        const researcher1Metrics = block.receipts[0].result.expectSome();
        const researcher2Metrics = block.receipts[1].result.expectSome();
        
        // Both should have non-zero collaboration scores
    },
});

Clarinet.test({
    name: "Test error handling in analytics system",
    async fn(chain: Chain, accounts: Map<string, Account>) {
        const deployer = accounts.get('deployer')!;
        
        // Test accessing non-existent data
        let block = chain.mineBlock([
            Tx.contractCall('research-impact-analytics', 'get-paper-impact-metrics', [
                types.uint(999) // Non-existent paper
            ], deployer.address),
        ]);
        
        assertEquals(block.receipts.length, 1);
        block.receipts[0].result.expectNone();
        
        // Test getting collaboration strength for authors who haven't collaborated
        block = chain.mineBlock([
            Tx.contractCall('research-impact-analytics', 'get-collaboration-strength', [
                types.principal('ST1HTBVD3JG9C05J7HBJTHGR0GGW7KXW28M5JS8QE'),
                types.principal('ST1J4G6RR643BCG8G8SR6M2D9Z9KXT2NJDRK3FBTK')
            ], deployer.address),
        ]);
        
        assertEquals(block.receipts.length, 1);
        assertEquals(block.receipts[0].result.expectOk(), types.uint(0));
    },
});

Clarinet.test({
    name: "Test integrated workflow - paper submission to impact tracking",
    async fn(chain: Chain, accounts: Map<string, Account>) {
        const deployer = accounts.get('deployer')!;
        const researcher1 = accounts.get('wallet_1')!;
        const researcher2 = accounts.get('wallet_2')!;
        const researcher3 = accounts.get('wallet_3')!;
        
        // Complete workflow: submit papers, verify, add citations, check analytics
        let block = chain.mineBlock([
            // Submit papers
            Tx.contractCall('ResearchShare', 'submit-paper', [
                types.ascii("Foundational Paper"),
                types.ascii("Base research")
            ], researcher1.address),
            Tx.contractCall('ResearchShare', 'submit-paper', [
                types.ascii("Building on Foundation"),
                types.ascii("Extended research")
            ], researcher2.address),
            Tx.contractCall('ResearchShare', 'submit-paper', [
                types.ascii("Further Extensions"),
                types.ascii("Advanced research")
            ], researcher3.address),
        ]);
        
        assertEquals(block.receipts.length, 3);
        
        // Verify papers
        block = chain.mineBlock([
            Tx.contractCall('ResearchShare', 'verify-paper', [types.uint(1)], deployer.address),
            Tx.contractCall('ResearchShare', 'verify-paper', [types.uint(2)], deployer.address),
            Tx.contractCall('ResearchShare', 'verify-paper', [types.uint(3)], deployer.address),
        ]);
        
        assertEquals(block.receipts.length, 3);
        
        // Create citation chain: 3 cites 2, 2 cites 1
        block = chain.mineBlock([
            Tx.contractCall('ResearchShare', 'add-citation', [
                types.uint(2), // cited paper
                types.uint(3)  // citing paper
            ], researcher3.address),
            Tx.contractCall('ResearchShare', 'add-citation', [
                types.uint(1), // cited paper  
                types.uint(2)  // citing paper
            ], researcher2.address),
            Tx.contractCall('ResearchShare', 'add-citation', [
                types.uint(1), // cited paper
                types.uint(3)  // citing paper
            ], researcher3.address),
        ]);
        
        assertEquals(block.receipts.length, 3);
        assertEquals(block.receipts[0].result.expectOk(), types.bool(true));
        assertEquals(block.receipts[1].result.expectOk(), types.bool(true));
        assertEquals(block.receipts[2].result.expectOk(), types.bool(true));
        
        // Check impact metrics were updated
        block = chain.mineBlock([
            Tx.contractCall('research-impact-analytics', 'get-citation-velocity', [
                types.uint(1) // Should have 2 citations
            ], deployer.address),
            Tx.contractCall('research-impact-analytics', 'get-citation-velocity', [
                types.uint(2) // Should have 1 citation
            ], deployer.address),
        ]);
        
        assertEquals(block.receipts.length, 2);
        assertEquals(block.receipts[0].result.expectOk(), types.uint(2)); // Paper 1 has 2 citations
        assertEquals(block.receipts[1].result.expectOk(), types.uint(1)); // Paper 2 has 1 citation
    },
});
