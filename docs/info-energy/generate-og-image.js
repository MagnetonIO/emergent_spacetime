const puppeteer = require('puppeteer');
const path = require('path');

async function generateOGImage() {
    const browser = await puppeteer.launch({
        headless: true,
        args: ['--no-sandbox', '--disable-setuid-sandbox']
    });
    
    const page = await browser.newPage();
    
    // Set viewport to exact OG image dimensions
    await page.setViewport({
        width: 1200,
        height: 630,
        deviceScaleFactor: 2 // For retina quality
    });
    
    // Load the HTML file
    const htmlPath = `file://${path.resolve(__dirname, 'og-image.html')}`;
    await page.goto(htmlPath, {
        waitUntil: 'networkidle0'
    });
    
    // Wait a bit for fonts to load
    await page.waitForTimeout(1000);
    
    // Take screenshot
    await page.screenshot({
        path: 'og-image.png',
        type: 'png',
        fullPage: false
    });
    
    console.log('OG image generated successfully: og-image.png');
    
    await browser.close();
}

generateOGImage().catch(console.error);