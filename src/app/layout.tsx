import type { Metadata } from 'next'
import { Inter, Orbitron } from 'next/font/google'
import './globals.css'
import { Navigation } from '@/components/Navigation'
import { Toaster } from '@/components/ui/toaster'
import { FrankProvider } from '@/contexts/FrankContext'
import { FrankMascot } from '@/components/FrankMascot'

const inter = Inter({
  subsets: ['latin'],
  variable: '--font-inter',
})

const orbitron = Orbitron({
  subsets: ['latin'],
  variable: '--font-display',
})

export const metadata: Metadata = {
  title: 'GameNight',
  description: 'Track scores for your game nights - Dominoes, Rummy, Mahjong and more!',
}

export default function RootLayout({
  children,
}: {
  children: React.ReactNode
}) {
  return (
    <html lang="en" className="dark">
      <body className={`${inter.variable} ${orbitron.variable} font-sans min-h-screen bg-background`}>
        <FrankProvider>
          <Navigation />
          <FrankMascot />
          <main className="container mx-auto px-4 py-6">
            {children}
          </main>
          <Toaster />
        </FrankProvider>
      </body>
    </html>
  )
}
