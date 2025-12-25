/** @type {import('next').NextConfig} */
const nextConfig = {
  images: {
    remotePatterns: [
      {
        protocol: 'https',
        hostname: '*.supabase.co',
        port: '',
        pathname: '/storage/v1/object/public/**',
      },
    ],
  },
  // Exclude only problematic patterns, not everything
  experimental: {
    outputFileTracingExcludes: {
      '/api/frank/dialogue': ['node_modules/**/*'],
    },
  },
}

module.exports = nextConfig
