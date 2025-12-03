import type { Metadata } from 'next'
import { Inter, Anta } from 'next/font/google'
import './globals.css'
import { Navigation } from '@/components/Navigation'
import { Toaster } from '@/components/ui/toaster'

const inter = Inter({
  subsets: ['latin'],
  variable: '--font-inter',
})

const anta = Anta({
  weight: '400',
  subsets: ['latin'],
  variable: '--font-anta',
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
      <body className={`${inter.variable} ${anta.variable} font-sans min-h-screen bg-background`}>
        <Navigation />
        <main className="container mx-auto px-4 py-6">
          {children}
        </main>
        <Toaster />
      </body>
    </html>
  )
}
