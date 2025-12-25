"use client"

import Link from 'next/link'
import Image from 'next/image'
import { usePathname } from 'next/navigation'
import { Home, Play, Users } from 'lucide-react'
import { cn } from '@/lib/utils'

const navItems = [
  { href: '/', label: 'Home', icon: Home },
  { href: '/play', label: 'Play', icon: Play },
  { href: '/players', label: 'Players', icon: Users },
]

export function Navigation() {
  const pathname = usePathname()

  return (
    <nav className="sticky top-0 z-50 w-full nav-arcade">
      <div className="container flex h-16 items-center justify-between">
        {/* Logo */}
        <Link href="/" className="flex items-center group">
          <div className="relative transition-all duration-300 group-hover:scale-105">
            <Image
              src="/images/neon.png"
              alt="GameNight"
              width={140}
              height={70}
              className="h-auto"
              priority
            />
          </div>
        </Link>

        {/* Nav Links */}
        <div className="flex items-center gap-1">
          {navItems.map((item) => {
            const Icon = item.icon
            const isActive = pathname === item.href ||
              (item.href !== '/' && pathname.startsWith(item.href))

            return (
              <Link
                key={item.href}
                href={item.href}
                className={cn(
                  "flex items-center gap-2 px-4 py-2 rounded-xl text-sm font-medium transition-all duration-300",
                  isActive
                    ? "bg-[#ff2d75]/15 text-[#ff2d75] shadow-[0_0_20px_rgba(255,45,117,0.2)]"
                    : "text-white/60 hover:text-white hover:bg-white/5"
                )}
              >
                <Icon className={cn(
                  "h-4 w-4 transition-all duration-300",
                  isActive && "drop-shadow-[0_0_8px_rgba(255,45,117,0.8)]"
                )} />
                <span>{item.label}</span>
              </Link>
            )
          })}
        </div>
      </div>
    </nav>
  )
}
